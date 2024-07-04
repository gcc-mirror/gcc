// Copyright (C) 2020-2024 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#ifndef RUST_OBJECT_EXPORT_H
#define RUST_OBJECT_EXPORT_H

#include "rust-system.h"

extern unsigned int
rust_field_alignment (tree t);

extern const char *
rust_read_export_data (int fd, off_t offset, char **pbuf, size_t *plen,
		       int *perr);
extern void
rust_write_export_data (const char *bytes, unsigned int size);

#endif // RUST_OBJECT_EXPORT_H
