/* Solaris support needed only by C/C++ frontends.
   Copyright (C) 2004  Free Software Foundation, Inc.
   Contributed by CodeSourcery, LLC.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"

#include "c-format.h"
#include "intl.h"

/* cmn_err only accepts "l" and "ll".  */
static const format_length_info cmn_err_length_specs[] =
{
  { "l", FMT_LEN_l, STD_C89, "ll", FMT_LEN_ll, STD_C89 },
  { NULL, 0, 0, NULL, 0, 0 }
};

static const format_flag_spec cmn_err_flag_specs[] =
{
  { 'w',  0, 0, N_("field width"),     N_("field width in printf format"),     STD_C89 },
  { 'L',  0, 0, N_("length modifier"), N_("length modifier in printf format"), STD_C89 },
  { 0, 0, 0, NULL, NULL, 0 }
};


static const format_flag_pair cmn_err_flag_pairs[] =
{
  { 0, 0, 0, 0 }
};

static const format_char_info bitfield_string_type =
  { "b",   1, STD_C89, { T89_C,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "",   "cR", NULL };

static const format_char_info cmn_err_char_table[] =
{
  /* C89 conversion specifiers.  */
  { "dD",  0, STD_C89, { T89_I,   BADLEN,  BADLEN,  T89_L,   T9L_LL,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "w",  "",   NULL },
  { "oOxX",0, STD_C89, { T89_UI,  BADLEN,  BADLEN,  T89_UL,  T9L_ULL, BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "w",  "",   NULL },
  { "u",   0, STD_C89, { T89_UI,  BADLEN,  BADLEN,  T89_UL,  T9L_ULL, BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "w",  "",   NULL },
  { "c",   0, STD_C89, { T89_C,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "w",  "",   NULL },
  { "s",   1, STD_C89, { T89_C,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "w",  "cR", NULL },
  { "b",   0, STD_C89, { T89_I,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "",   "",   &bitfield_string_type },
  { NULL,  0, 0, NOLENGTHS, NULL, NULL, NULL }
};

const format_kind_info solaris_format_types[] = {
  { "cmn_err",  cmn_err_length_specs,  cmn_err_char_table, "", NULL,
    cmn_err_flag_specs, cmn_err_flag_pairs,
    FMT_FLAG_ARG_CONVERT|FMT_FLAG_EMPTY_PREC_OK,
    'w', 0, 0, 0, 'L',
    &integer_type_node, &integer_type_node
  }
};
