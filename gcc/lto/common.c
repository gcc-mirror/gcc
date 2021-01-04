/* Common code for the plugin and lto1.
   Copyright (C) 2009-2021 Free Software Foundation, Inc.
   Contributed by Rafael Avila de Espindola (espindola@google.com).

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA
02110-1301, USA.  */

#include "common.h"

const char *lto_kind_str[5] __attribute__ ((visibility ("hidden"))) =
{
  "DEF", "WEAKDEF", "UNDEF",
  "WEAKUNDEF", "COMMON"
};

const char *lto_visibility_str[4] __attribute__ ((visibility ("hidden"))) =
{
  "DEFAULT", "PROTECTED",
 "INTERNAL", "HIDDEN"
};

const char *lto_resolution_str[10] __attribute__ ((visibility ("hidden"))) =
{
  "UNKNOWN",
  "UNDEF",
  "PREVAILING_DEF",
  "PREVAILING_DEF_IRONLY",
  "PREEMPTED_REG",
  "PREEMPTED_IR",
  "RESOLVED_IR",
  "RESOLVED_EXEC",
  "RESOLVED_DYN",
  "PREVAILING_DEF_IRONLY_EXP",
};

