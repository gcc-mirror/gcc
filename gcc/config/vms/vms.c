/* Definitions of target machine GNU compiler. 32bit VMS version.
   Copyright (C) 2009, 2010 Free Software Foundation, Inc.
   Contributed by Douglas B Rupp (rupp@gnat.com).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "vms-protos.h"
#include "tm.h"
#include "ggc.h"

/* Correlation of standard CRTL names with DECCRTL function names.  */

/* Name is for a function that allocate memory.  Use the 64bit version
   if -mmalloc64.  */
#define VMS_CRTL_MALLOC	(1 << 0)

/* If long pointer are enabled, use _NAME64 instead.  */
#define VMS_CRTL_64	(1 << 1)

/* Use tNAME instead.  To be applied after the previous rule.  */
#define VMS_CRTL_FLOAT  (1 << 2)

/* Prepend __bsd44__ before the name.  To be applied after the P64
   rule.  */
#define VMS_CRTL_BSD44	(1 << 3)

/* Prepend x before the name for printf like functions.  */
#define VMS_CRTL_PRNTF	(1 << 4)

/* Prepend ga_ for global data.  */
#define VMS_CRTL_GLOBAL (1 << 5)

struct vms_crtl_name
{
  /* The standard C name.  */
  const char *const name;

  /* Flags to drive the translation.  */
  unsigned int flags;
};

/* Map for the translation.  */

static const struct vms_crtl_name vms_crtl_names[] =
  {
#include "vms-crtlmap.h"
  };

/* Number of entires in the above array.  */

#define NBR_CRTL_NAMES (sizeof (vms_crtl_names) / sizeof (*vms_crtl_names))

/* List of aliased identifiers.  They must be persistant accross gc.  */

static GTY(()) VEC(tree,gc) *aliases_id;

/* Add a CRTL translation.  This simply use the transparent alias
   mechanism, which is platform independant and works with the
   #pragma extern_prefix (which set the assembler name).  */

static void
vms_add_crtl_xlat (const char *name, size_t nlen,
                   const char *id_str, size_t id_len)
{
  tree targ;

  targ = get_identifier_with_length (name, nlen);
  gcc_assert (!IDENTIFIER_TRANSPARENT_ALIAS (targ));
  IDENTIFIER_TRANSPARENT_ALIAS (targ) = 1;
  TREE_CHAIN (targ) = get_identifier_with_length (id_str, id_len);

  VEC_safe_push (tree, gc, aliases_id, targ);

  /* printf ("vms: %s (%p) -> %.*s\n", name, targ, id_len, id_str); */
}

/* Do VMS specific stuff on builtins: disable the ones that are not
   standard, mangle names.  */

void
vms_patch_builtins (void)
{
  /* enum built_in_function bi; */
  unsigned int i;

  /* Fwrite on VMS is non-standard.  */
  if (builtin_decl_implicit_p (BUILT_IN_FWRITE))
    set_builtin_decl_implicit_p (BUILT_IN_FWRITE, false);

  if (builtin_decl_implicit_p (BUILT_IN_FWRITE_UNLOCKED))
    set_builtin_decl_implicit_p (BUILT_IN_FWRITE_UNLOCKED, false);

  /* Define aliases for names.  */
  for (i = 0; i < NBR_CRTL_NAMES; i++)
    {
      const struct vms_crtl_name *n = &vms_crtl_names[i];
      char res[VMS_CRTL_MAXLEN + 3 + 9 + 1 + 1];
      int rlen;
      int nlen;

      /* Add the dec-c prefix.  */
      memcpy (res, "decc$", 5);
      rlen = 5;

      if (n->flags & VMS_CRTL_BSD44)
        {
          memcpy (res + rlen, "__bsd44__", 9);
          rlen += 9;
        }

      if (n->flags & VMS_CRTL_GLOBAL)
        {
          memcpy (res + rlen, "ga_", 3);
          rlen += 3;
        }

      if (n->flags & VMS_CRTL_FLOAT)
        res[rlen++] = 't';

      if (n->flags & VMS_CRTL_PRNTF)
        res[rlen++] = 'x';

      nlen = strlen (n->name);
      memcpy (res + rlen, n->name, nlen);

      if ((n->flags & VMS_CRTL_64) == 0)
        vms_add_crtl_xlat (n->name, nlen, res, rlen + nlen);
      else
        {
          char alt[VMS_CRTL_MAXLEN + 3];
          bool use_64;

          /* Add three translations:
             _X32 -> X
             _X64 -> _X64
             X -> X if short, _X64 if long.  */
          alt[0] = '_';
          memcpy (alt + 1, n->name, nlen);
          alt[1 + nlen + 0] = '3';
          alt[1 + nlen + 1] = '2';
          alt[1 + nlen + 2] = 0;
          vms_add_crtl_xlat (alt, nlen + 3, res, rlen + nlen);

          use_64 = (((n->flags & VMS_CRTL_64) && POINTER_SIZE == 64)
                    || ((n->flags & VMS_CRTL_MALLOC)
                        && TARGET_MALLOC64));
          if (!use_64)
            vms_add_crtl_xlat (n->name, nlen, res, rlen + nlen);

          res[rlen++] = '_';
          memcpy (res + rlen, n->name, nlen);
          res[rlen + nlen + 0] = '6';
          res[rlen + nlen + 1] = '4';

          if (use_64)
            vms_add_crtl_xlat (n->name, nlen, res, rlen + nlen + 2);

          alt[1 + nlen + 0] = '6';
          alt[1 + nlen + 1] = '4';
          vms_add_crtl_xlat (alt, nlen + 3, res, rlen + nlen + 2);
        }
    }
}

#include "gt-vms.h"
