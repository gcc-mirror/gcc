/* Default common target hook functions.
   Copyright (C) 2003-2018 Free Software Foundation, Inc.

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

#ifndef GCC_COMMON_TARGHOOKS_H
#define GCC_COMMON_TARGHOOKS_H

extern enum unwind_info_type default_except_unwind_info (struct gcc_options *);
extern enum unwind_info_type dwarf2_except_unwind_info (struct gcc_options *);
extern enum unwind_info_type sjlj_except_unwind_info (struct gcc_options *);

extern bool default_target_handle_option (struct gcc_options *,
					  struct gcc_options *,
					  const struct cl_decoded_option *,
					  location_t);
extern vec<const char *> default_get_valid_option_values (int, const char *);

extern bool default_option_validate_param (const int, const int);

extern const struct default_options empty_optimization_table[];

#endif
