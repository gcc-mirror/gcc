/* Check calls to formatted I/O functions (-Wformat).
   Copyright (C) 1992-2019 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "c-family/c-common.h"
#include "intl.h"
#include "c-family/c-format.h"

/* Mingw specific format attributes ms_printf, ms_scanf, and ms_strftime.  */

static format_length_info ms_printf_length_specs[] =
{
  { "h", FMT_LEN_h, STD_C89, NULL, FMT_LEN_none, STD_C89, 0 },
  { "l", FMT_LEN_l, STD_C89, NULL, FMT_LEN_none, STD_C89, 0 },
  { "I32", FMT_LEN_l, STD_EXT, NULL, FMT_LEN_none, STD_C89, 1 },
  { "I64", FMT_LEN_ll, STD_EXT, NULL, FMT_LEN_none, STD_C89, 1 },
  { "I", FMT_LEN_L, STD_EXT, NULL, FMT_LEN_none, STD_C89, 1 },
  { NULL, FMT_LEN_none, STD_C89, NULL, FMT_LEN_none, STD_C89, 0 }
};

static const format_flag_spec ms_printf_flag_specs[] =
{
  { ' ',  0, 0, 0, N_("' ' flag"),        N_("the ' ' printf flag"),              STD_C89 },
  { '+',  0, 0, 0, N_("'+' flag"),        N_("the '+' printf flag"),              STD_C89 },
  { '#',  0, 0, 0, N_("'#' flag"),        N_("the '#' printf flag"),              STD_C89 },
  { '0',  0, 0, 0, N_("'0' flag"),        N_("the '0' printf flag"),              STD_C89 },
  { '-',  0, 0, 0, N_("'-' flag"),        N_("the '-' printf flag"),              STD_C89 },
  { '\'', 0, 0, 0, N_("''' flag"),        N_("the ''' printf flag"),              STD_EXT },
  { 'w',  0, 0, 0, N_("field width"),     N_("field width in printf format"),     STD_C89 },
  { 'p',  0, 0, 0, N_("precision"),       N_("precision in printf format"),       STD_C89 },
  { 'L',  0, 0, 0, N_("length modifier"), N_("length modifier in printf format"), STD_C89 },
  { 0, 0, 0, 0, NULL, NULL, STD_C89 }
};

static const format_flag_pair ms_printf_flag_pairs[] =
{
  { ' ', '+', 1, 0   },
  { '0', '-', 1, 0   }, { '0', 'p', 1, 'i' },
  { 0, 0, 0, 0 }
};

static const format_flag_spec ms_scanf_flag_specs[] =
{
  { '*',  0, 0, 0, N_("assignment suppression"), N_("the assignment suppression scanf feature"), STD_C89 },
  { 'a',  0, 0, 0, N_("'a' flag"),               N_("the 'a' scanf flag"),                       STD_EXT },
  { 'w',  0, 0, 0, N_("field width"),            N_("field width in scanf format"),              STD_C89 },
  { 'L',  0, 0, 0, N_("length modifier"),        N_("length modifier in scanf format"),          STD_C89 },
  { '\'', 0, 0, 0, N_("''' flag"),               N_("the ''' scanf flag"),                       STD_EXT },
  { 0, 0, 0, 0, NULL, NULL, STD_C89 }
};

static const format_flag_pair ms_scanf_flag_pairs[] =
{
  { '*', 'L', 0, 0 },
  { 0, 0, 0, 0 }
};

static const format_flag_spec ms_strftime_flag_specs[] =
{
  { '#', 0,   0, 0, N_("'#' flag"),     N_("the '#' strftime flag"),          STD_EXT },
  { 0, 0, 0, 0, NULL, NULL, STD_C89 }
};

static const format_flag_pair ms_strftime_flag_pairs[] =
{
  { 0, 0, 0, 0 }
};

static const format_char_info ms_print_char_table[] =
{
  /* C89 conversion specifiers.  */
  { "di",  0, STD_C89, { T89_I,   BADLEN,  T89_S,   T89_L,   T9L_LL,  T99_SST,  BADLEN, BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "-wp0 +'",  "i",  NULL },
  { "oxX", 0, STD_C89, { T89_UI,  BADLEN,  T89_US,  T89_UL,  T9L_ULL, T99_ST, BADLEN, BADLEN, BADLEN, BADLEN,  BADLEN,  BADLEN }, "-wp0#",     "i",  NULL },
  { "u",   0, STD_C89, { T89_UI,  BADLEN,  T89_US,  T89_UL,  T9L_ULL, T99_ST, BADLEN, BADLEN, BADLEN, BADLEN,  BADLEN,  BADLEN }, "-wp0'",    "i",  NULL },
  { "fgG", 0, STD_C89, { T89_D,   BADLEN,  BADLEN,  T99_D,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN, BADLEN, BADLEN }, "-wp0 +#'", "",   NULL },
  { "eE",  0, STD_C89, { T89_D,   BADLEN,  BADLEN,  T99_D,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN, BADLEN, BADLEN }, "-wp0 +#",  "",   NULL },
  { "c",   0, STD_C89, { T89_I,   BADLEN,  T89_S,  T94_WI,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "-w",        "",   NULL },
  { "s",   1, STD_C89, { T89_C,   BADLEN,  T89_S,  T94_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "-wp",       "cR", NULL },
  { "p",   1, STD_C89, { T89_V,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "-w",        "c",  NULL },
  { "n",   1, STD_C89, { T89_I,   BADLEN,  T89_S,   T89_L,   T9L_LL,  BADLEN,  BADLEN, BADLEN,  T99_IM,  BADLEN,  BADLEN,  BADLEN }, "",          "W",  NULL },
  /* X/Open conversion specifiers.  */
  { "C",   0, STD_EXT, { TEX_WI,  BADLEN,  T89_S,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "-w",        "",   NULL },
  { "S",   1, STD_EXT, { TEX_W,   BADLEN,  T89_S,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "-wp",       "R",  NULL },
  { NULL,  0, STD_C89, NOLENGTHS, NULL, NULL, NULL }
};

static const format_char_info ms_scan_char_table[] =
{
  /* C89 conversion specifiers.  */
  { "di",    1, STD_C89, { T89_I,   BADLEN,  T89_S,   T89_L,   T9L_LL,  T99_SST,  BADLEN, BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "*w'", "W",   NULL },
  { "u",     1, STD_C89, { T89_UI,  BADLEN,  T89_US,  T89_UL,  T9L_ULL, T99_ST, BADLEN,  BADLEN, BADLEN, BADLEN,  BADLEN,  BADLEN }, "*w'", "W",   NULL },
  { "oxX",   1, STD_C89, { T89_UI,  BADLEN,  T89_US,  T89_UL,  T9L_ULL, T99_ST, BADLEN,  BADLEN, BADLEN, BADLEN,  BADLEN,  BADLEN }, "*w",   "W",   NULL },
  { "efgEG", 1, STD_C89, { T89_F,   BADLEN,  BADLEN,  T89_D,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN, BADLEN, BADLEN }, "*w'",  "W",   NULL },
  { "c",     1, STD_C89, { T89_C,   BADLEN,  T89_S,  T94_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "*w",   "cW",  NULL },
  { "s",     1, STD_C89, { T89_C,   BADLEN,  T89_S,  T94_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "*aw",  "cW",  NULL },
  { "[",     1, STD_C89, { T89_C,   BADLEN,  BADLEN,  T94_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "*aw",  "cW[", NULL },
  { "p",     2, STD_C89, { T89_V,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "*w",   "W",   NULL },
  { "n",     1, STD_C89, { T89_I,   BADLEN,  T89_S,   T89_L,   T9L_LL,  BADLEN,  BADLEN, BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "",     "W",   NULL },
  /* X/Open conversion specifiers.  */
  { "C",     1, STD_EXT, { TEX_W,   BADLEN,  T89_S,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "*w",   "W",   NULL },
  { "S",     1, STD_EXT, { TEX_W,   BADLEN,  T89_S,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "*aw",  "W",   NULL },
  { NULL, 0, STD_C89, NOLENGTHS, NULL, NULL, NULL }
};

static const format_char_info ms_time_char_table[] =
{
  /* C89 conversion specifiers.  */
  { "ABZab",		0, STD_C89, NOLENGTHS, "#",     "",   NULL },
  { "cx",		0, STD_C89, NOLENGTHS, "#",      "3",  NULL },
  { "HIMSUWdmw",	0, STD_C89, NOLENGTHS, "#",  "",   NULL },
  { "j",		0, STD_C89, NOLENGTHS, "#",  "",  NULL },
  { "p",		0, STD_C89, NOLENGTHS, "#",      "",   NULL },
  { "X",		0, STD_C89, NOLENGTHS, "#",      "",   NULL },
  { "y",		0, STD_C89, NOLENGTHS, "#", "4",  NULL },
  { "Y",		0, STD_C89, NOLENGTHS, "#", "",  NULL },
  { "%",		0, STD_C89, NOLENGTHS, "",       "",   NULL },
  /* C99 conversion specifiers.  */
  { "z",		0, STD_C99, NOLENGTHS, "#",      "",  NULL },
  { NULL,		0, STD_C89, NOLENGTHS, NULL, NULL, NULL }
};

EXPORTED_CONST format_kind_info mingw_format_attributes[3] =
{
  { "ms_printf",   ms_printf_length_specs,  ms_print_char_table, " +#0-'", NULL,
    ms_printf_flag_specs, ms_printf_flag_pairs,
    FMT_FLAG_ARG_CONVERT|FMT_FLAG_DOLLAR_MULTIPLE|FMT_FLAG_USE_DOLLAR|FMT_FLAG_EMPTY_PREC_OK,
    'w', 0, 'p', 0, 'L', 0,
    &integer_type_node, &integer_type_node
  },
  { "ms_scanf",    ms_printf_length_specs,   ms_scan_char_table,  "*'", NULL,
    ms_scanf_flag_specs, ms_scanf_flag_pairs,
    FMT_FLAG_ARG_CONVERT|FMT_FLAG_SCANF_A_KLUDGE|FMT_FLAG_USE_DOLLAR|FMT_FLAG_ZERO_WIDTH_BAD|FMT_FLAG_DOLLAR_GAP_POINTER_OK,
    'w', 0, 0, '*', 'L', 0,
    NULL, NULL
  },
  { "ms_strftime", NULL,                 ms_time_char_table,  "", "#",
    ms_strftime_flag_specs, ms_strftime_flag_pairs,
    FMT_FLAG_FANCY_PERCENT_OK, 0, 0, 0, 0, 0, 0,
    NULL, NULL
  }
};

/* Default overrides for printf, scanf and strftime.  */
EXPORTED_CONST target_ovr_attr mingw_format_attribute_overrides[4] =
{
  { "ms_printf", "printf" },
  { "ms_scanf", "scanf" },
  { "ms_strftime", "strftime" }
};

/* Setup for option Wpedantic-ms-format.  */

#ifdef TARGET_OVERRIDES_FORMAT_INIT

/* Make sure TARGET_OVERRIDES_FORMAT_INIT is prototyped.  */
extern void TARGET_OVERRIDES_FORMAT_INIT (void);

/* Helper.  */
#define C89_OR_EXT (warn_pedantic_ms_format ? STD_EXT : STD_C89)

void
TARGET_OVERRIDES_FORMAT_INIT (void)
{
  ms_printf_length_specs[2].std = C89_OR_EXT; /* I32 */
  ms_printf_length_specs[3].std = C89_OR_EXT; /* I64 */
  ms_printf_length_specs[4].std = C89_OR_EXT; /* I */
}

#undef C89_OR_EXT

#endif
