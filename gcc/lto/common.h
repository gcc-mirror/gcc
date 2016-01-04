/* Common code for the plugin and lto1.
   Copyright (C) 2008-2016 Free Software Foundation, Inc.
   Contributed by Rafael Avila de Espindola (espindola@google.com).

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



static const char *lto_resolution_str[10] =
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
