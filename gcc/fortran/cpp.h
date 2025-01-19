/* Copyright (C) 2008-2025 Free Software Foundation, Inc.

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

#ifndef GFC_CPP_H
#define GFC_CPP_H

/* Returns true if preprocessing is enabled, false otherwise.  */
bool gfc_cpp_enabled (void);

bool gfc_cpp_preprocess_only (void);

bool gfc_cpp_makedep (void);

void gfc_cpp_add_dep (const char *name, bool system);

void gfc_cpp_add_target (const char *name);

const char *gfc_cpp_temporary_file (void);


void gfc_cpp_init_0 (void);
void gfc_cpp_init (void);

void gfc_cpp_init_options (unsigned int decoded_options_count,
			   struct cl_decoded_option *decoded_options);

bool gfc_cpp_handle_option(size_t scode, const char *arg, int value);

void gfc_cpp_post_options (bool);

bool gfc_cpp_preprocess (const char *source_file);

void gfc_cpp_done (void);

void gfc_cpp_add_include_path (char *path, bool user_supplied);
void gfc_cpp_add_include_path_after (char *path, bool user_supplied);

#endif /* GFC_CPP_H */
