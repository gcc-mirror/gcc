/* Routines to parse the AArch64 tuning parameters from a JSON file.
   Copyright The GNU Toolchain Authors.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef AARCH64_JSON_TUNINGS_PARSER_H
#define AARCH64_JSON_TUNINGS_PARSER_H

#include "aarch64-protos.h"

void
aarch64_load_tuning_params_from_json (const char *data_filename,
				      struct tune_params *tune);

#endif