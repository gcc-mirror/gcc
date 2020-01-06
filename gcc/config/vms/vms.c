/* Definitions of target machine GNU compiler. 32bit VMS version.
   Copyright (C) 2009-2020 Free Software Foundation, Inc.
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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tree.h"
#include "stringpool.h"
#include "alias.h"
#include "vms-protos.h"
#include "output.h"
#include "dwarf2out.h"

/* Correlation of standard CRTL names with DECCRTL function names.  */

/* Name is for a function that allocate memory.  Use the 64bit version
   if -mmalloc64.  */
#define VMS_CRTL_MALLOC	(1 << 0)

/* If long pointer are enabled, use _NAME64 instead.  */
#define VMS_CRTL_64	(1 << 1)

/* Prepend s/f before the name.  To be applied after the previous rule.
   use 's' for S float, 'f' for IEEE 32.  */
#define VMS_CRTL_FLOAT32  (1 << 2)

/* Prepend t/g/d before the name.  To be applied after the previous rule.
   use 'g' for VAX G float, 'd' for VAX D float, 't' for IEEE 64.  */
#define VMS_CRTL_FLOAT64  (1 << 3)

/* Prepend d before the name, only if using VAX fp.  */
#define VMS_CRTL_FLOAT64_VAXD  (1 << 4)

/* Prepend x before the name for if 128 bit long doubles are enabled.  This
   concern mostly 'printf'-like functions.  */
#define VMS_CRTL_FLOAT128 (1 << 5)

/* From xxx, create xxx, xxxf, xxxl using MATH$XXX_T, MATH$XXX_S
   and MATH$XXX{_X} if DPML is used.  */
#define VMS_CRTL_DPML (1 << 6)

/* Together with DPML, it means that all variant (ie xxx, xxxf and xxxl) are
   overridden by decc.  Without DPML, it means this is a variant (ie xxxf
   or xxxl) of a function.  */
#define VMS_CRTL_NODPML (1 << 7)

/* Prepend __bsd44_ before the name.  To be applied after the P64
   rule.  */
#define VMS_CRTL_BSD44	(1 << 8)

/* Define only in 32 bits mode, as this has no 64 bit variants.
   Concerns getopt/getarg.  */
#define VMS_CRTL_32ONLY (1 << 9)

/* GLobal data prefix (ga_, gl_...)  */
#define VMS_CRTL_G_MASK (7 << 10)
#define VMS_CRTL_G_NONE (0 << 10)
#define VMS_CRTL_GA	(1 << 10)
#define VMS_CRTL_GL	(2 << 10)

/* Append '_2'.  Not compatible with 64.  */
#define VMS_CRTL_FLOATV2 (1 << 13)

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

/* List of aliased identifiers.  They must be persistent across gc.  */

static GTY(()) vec<tree, va_gc> *aliases_id;

/* Add a CRTL translation.  This simply use the transparent alias
   mechanism, which is platform independent and works with the
   #pragma extern_prefix (which set the assembler name).  */

static void
vms_add_crtl_xlat (const char *name, size_t nlen,
                   const char *id_str, size_t id_len)
{
  tree targ;

  /* printf ("vms crtl: %.*s -> %.*s\n", nlen, name, id_len, id_str); */

  targ = get_identifier_with_length (name, nlen);
  gcc_assert (!IDENTIFIER_TRANSPARENT_ALIAS (targ));
  IDENTIFIER_TRANSPARENT_ALIAS (targ) = 1;
  TREE_CHAIN (targ) = get_identifier_with_length (id_str, id_len);

  vec_safe_push (aliases_id, targ);
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
      int nlen = strlen (n->name);

      /* Discard 32ONLY if using 64 bit pointers.  */
      if ((n->flags & VMS_CRTL_32ONLY)
	  && flag_vms_pointer_size == VMS_POINTER_SIZE_64)
	continue;

      /* Handle DPML unless overridden by decc.  */
      if ((n->flags & VMS_CRTL_DPML)
	  && !(n->flags & VMS_CRTL_NODPML))
	{
	  const char *p;
          char alt[VMS_CRTL_MAXLEN + 3];

	  memcpy (res, "MATH$", 5);
	  rlen = 5;
	  for (p = n->name; *p; p++)
	    res[rlen++] = TOUPPER (*p);
	  res[rlen++] = '_';
	  res[rlen++] = 'T';

	  /* Double version.  */
	  if (!(n->flags & VMS_CRTL_FLOAT64))
	    vms_add_crtl_xlat (n->name, nlen, res, rlen);

	  /* Float version.  */
	  res[rlen - 1] = 'S';
	  memcpy (alt, n->name, nlen);
	  alt[nlen] = 'f';
	  vms_add_crtl_xlat (alt, nlen + 1, res, rlen);

	  /* Long double version.  */
	  res[rlen - 1] = (LONG_DOUBLE_TYPE_SIZE == 128 ? 'X' : 'T');
	  alt[nlen] = 'l';
	  vms_add_crtl_xlat (alt, nlen + 1, res, rlen);

	  if (!(n->flags & (VMS_CRTL_FLOAT32 | VMS_CRTL_FLOAT64)))
	    continue;
	}

      if (n->flags & VMS_CRTL_FLOAT64_VAXD)
	continue;

      /* Add the dec-c prefix.  */
      memcpy (res, "decc$", 5);
      rlen = 5;

      if (n->flags & VMS_CRTL_BSD44)
        {
          memcpy (res + rlen, "__bsd44_", 8);
          rlen += 8;
        }

      if ((n->flags & VMS_CRTL_G_MASK) != VMS_CRTL_G_NONE)
        {
	  res[rlen++] = 'g';
	  switch (n->flags & VMS_CRTL_G_MASK)
	    {
	    case VMS_CRTL_GA:
	      res[rlen++] = 'a';
	      break;
	    case VMS_CRTL_GL:
	      res[rlen++] = 'l';
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  res[rlen++] = '_';
        }

      if (n->flags & VMS_CRTL_FLOAT32)
        res[rlen++] = 'f';

      if (n->flags & VMS_CRTL_FLOAT64)
        res[rlen++] = 't';

      if ((n->flags & VMS_CRTL_FLOAT128) && LONG_DOUBLE_TYPE_SIZE == 128)
        res[rlen++] = 'x';

      memcpy (res + rlen, n->name, nlen);

      if ((n->flags & VMS_CRTL_64) == 0)
	{
	  rlen += nlen;

	  if (n->flags & VMS_CRTL_FLOATV2)
	    {
	      res[rlen++] = '_';
	      res[rlen++] = '2';
	    }
	  vms_add_crtl_xlat (n->name, nlen, res, rlen);
	}
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

          use_64 = (((n->flags & VMS_CRTL_64)
                     && flag_vms_pointer_size == VMS_POINTER_SIZE_64)
                    || ((n->flags & VMS_CRTL_MALLOC)
                        && flag_vms_malloc64
                        && flag_vms_pointer_size != VMS_POINTER_SIZE_NONE));
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

/* Always default to .text section.  */

section *
vms_function_section (tree decl ATTRIBUTE_UNUSED,
                      enum node_frequency freq ATTRIBUTE_UNUSED,
                      bool startup ATTRIBUTE_UNUSED,
                      bool exit ATTRIBUTE_UNUSED)
{
  return NULL;
}

/* Additionnal VMS specific code for start_function.  */

/* Must be kept in sync with libgcc/config/vms/vms-ucrt0.c  */
#define VMS_MAIN_FLAGS_SYMBOL "__gcc_main_flags"
#define MAIN_FLAG_64BIT (1 << 0)
#define MAIN_FLAG_POSIX (1 << 1)

void
vms_start_function (const char *fnname)
{
#if VMS_DEBUGGING_INFO
  if (vms_debug_main
      && debug_info_level > DINFO_LEVEL_NONE
      && strncmp (vms_debug_main, fnname, strlen (vms_debug_main)) == 0)
    {
      targetm.asm_out.globalize_label (asm_out_file, VMS_DEBUG_MAIN_POINTER);
      ASM_OUTPUT_DEF (asm_out_file, VMS_DEBUG_MAIN_POINTER, fnname);
      dwarf2out_vms_debug_main_pointer ();
      vms_debug_main = 0;
    }
#endif

  /* Registers flags used for function main.  This is necessary for
     crt0 code.  */
  if (strcmp (fnname, "main") == 0)
    {
      unsigned int flags = 0;

      if (flag_vms_pointer_size == VMS_POINTER_SIZE_64)
	flags |= MAIN_FLAG_64BIT;
      if (!flag_vms_return_codes)
	flags |= MAIN_FLAG_POSIX;

      targetm.asm_out.globalize_label (asm_out_file, VMS_MAIN_FLAGS_SYMBOL);
      assemble_name (asm_out_file, VMS_MAIN_FLAGS_SYMBOL);
      fprintf (asm_out_file, " = %u\n", flags);
    }
}

#include "gt-vms.h"
