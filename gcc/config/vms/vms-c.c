/* VMS specific, C compiler specific functions.
   Copyright (C) 2011
   Free Software Foundation, Inc.
   Contributed by Tristan Gingold (gingold@adacore.com).

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
#include "tm.h"
#include "cpplib.h"
#include "tree.h"
#include "c-family/c-pragma.h"
#include "c-family/c-common.h"
#include "toplev.h"
#include "ggc.h"
#include "tm_p.h"
#include "incpath.h"
#include "diagnostic.h"

/* '#pragma __nostandard' is simply ignored.  */

static void
vms_pragma_nostandard (cpp_reader *pfile ATTRIBUTE_UNUSED)
{
  tree x;

  if (pragma_lex (&x) != CPP_EOF)
    warning (OPT_Wpragmas, "junk at end of #pragma __nostandard");
}

/* '#pragma __standard' is simply ignored.  */

static void
vms_pragma_standard (cpp_reader *pfile ATTRIBUTE_UNUSED)
{
  tree x;

  if (pragma_lex (&x) != CPP_EOF)
    warning (OPT_Wpragmas, "junk at end of #pragma __standard");
}

/* Saved member alignment.  */
static int saved_member_alignment;

/* Handle '#pragma member_alignment'.  */

static void
vms_pragma_member_alignment (cpp_reader *pfile ATTRIBUTE_UNUSED)
{
  tree x;
  int tok;
  const char *arg;

  tok = pragma_lex (&x);

  if (tok == CPP_EOF)
    {
      /* Disable packing.  */
      maximum_field_alignment = initial_max_fld_align;
      return;
    }
  if (tok != CPP_NAME)
    {
      warning (OPT_Wpragmas, "malformed '#pragma member_alignment', ignoring");
      return;
    }

  arg = IDENTIFIER_POINTER (x);
  /* Accept '__' prefix.  */
  if (arg[0] == '_' && arg[1] == '_')
    arg += 2;

  if (strcmp (arg, "save") == 0)
    saved_member_alignment = maximum_field_alignment;
  else if (strcmp (arg, "restore") == 0)
    maximum_field_alignment = saved_member_alignment;
  else
    {
      error ("unknown '#pragma member_alignment' name %s", arg);
      return;
    }
  if (pragma_lex (&x) != CPP_EOF)
    {
      error ("malformed '#pragma member_alignment'");
      return;
    }
}

/* Handle '#pragma nomember_alignment'.  */

static void
vms_pragma_nomember_alignment (cpp_reader *pfile ATTRIBUTE_UNUSED)
{
  tree x;
  int tok;

  tok = pragma_lex (&x);
  if (tok == CPP_NAME)
    {
      const char *arg = IDENTIFIER_POINTER (x);

      /* Accept '__' prefix.  */
      if (arg[0] == '_' && arg[1] == '_')
        arg += 2;

      if (strcmp (arg, "word") == 0)
        maximum_field_alignment = 2 * BITS_PER_UNIT;
      else if (strcmp (arg, "longword") == 0)
        maximum_field_alignment = 4 * BITS_PER_UNIT;
      else if (strcmp (arg, "quadword") == 0)
        maximum_field_alignment = 8 * BITS_PER_UNIT;
      else
        {
          error ("unhandled alignment for '#pragma nomember_alignment'");
        }

      tok = pragma_lex (&x);
    }
  else
    {
      /* Enable packing.  */
      maximum_field_alignment = BITS_PER_UNIT;
    }

  if (tok != CPP_EOF)
    {
      error ("garbage at end of '#pragma nomember_alignment'");
      return;
    }
}

/* The 'extern model' for public data.  */

enum extern_model_kind
{
  /* Create one overlaid section per variable.  */
  extern_model_common_block,

  /* Like unix: multiple not-initialized declarations are allowed.  */
  extern_model_relaxed_refdef,

  /* Like -fno-common.  */
  extern_model_strict_refdef,

  /* Declarations creates symbols without storage.  */
  extern_model_globalvalue
};

/* Current and saved extern model.  */
static enum extern_model_kind current_extern_model;
static enum extern_model_kind saved_extern_model;

/* Partial handling of '#pragma extern_model'.  */

static void
vms_pragma_extern_model (cpp_reader *pfile ATTRIBUTE_UNUSED)
{
  tree x;
  int tok;
  const char *arg;

  tok = pragma_lex (&x);

  if (tok != CPP_NAME)
    {
      warning (OPT_Wpragmas, "malformed '#pragma extern_model', ignoring");
      return;
    }

  arg = IDENTIFIER_POINTER (x);
  /* Accept "__" prefix.  */
  if (arg[0] == '_' && arg[1] == '_')
    arg += 2;

  if (strcmp (arg, "save") == 0)
    saved_extern_model = current_extern_model;
  else if (strcmp (arg, "restore") == 0)
    current_extern_model = saved_extern_model;
  else if (strcmp (arg, "strict_refdef") == 0)
    current_extern_model = extern_model_strict_refdef;
  else if (strcmp (arg, "common_block") == 0)
    current_extern_model = extern_model_common_block;
  else if (strcmp (arg, "globalvalue") == 0)
    {
      sorry ("extern model globalvalue");
      return;
    }
  else
    {
      error ("unknown '#pragma extern_model' model '%s'", arg);
      return;
    }
#if 0
  if (pragma_lex (&x) != CPP_EOF)
    {
      permerror (input_location, "junk at end of '#pragma extern_model'");
      return;
    }
#endif
}

/* Ignore '#pragma message'.  */

static void
vms_pragma_message (cpp_reader *pfile ATTRIBUTE_UNUSED)
{
  /* Completly ignored.  */
#if 0
  pedwarn (input_location, OPT_Wpragmas,
           "vms '#pragma __message' is ignored");
#endif
}

/* Add vms-specific pragma.  */

void
vms_c_register_pragma (void)
{
  c_register_pragma (NULL, "__nostandard", vms_pragma_nostandard);
  c_register_pragma (NULL, "nostandard", vms_pragma_nostandard);
  c_register_pragma (NULL, "__standard", vms_pragma_standard);
  c_register_pragma (NULL, "standard", vms_pragma_standard);
  c_register_pragma (NULL, "__member_alignment", vms_pragma_member_alignment);
  c_register_pragma (NULL, "member_alignment", vms_pragma_member_alignment);
  c_register_pragma (NULL, "__nomember_alignment",
                     vms_pragma_nomember_alignment);
  c_register_pragma (NULL, "nomember_alignment",
                     vms_pragma_nomember_alignment);
  c_register_pragma (NULL, "__extern_model", vms_pragma_extern_model);
  c_register_pragma (NULL, "extern_model", vms_pragma_extern_model);
  c_register_pragma (NULL, "__message", vms_pragma_message);
}
