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

// rust-location.h -- GCC specific Location declaration.   -*- C++ -*-

#ifndef RUST_LOCATION_H
#define RUST_LOCATION_H

#include "rich-location.h"
#include "rust-system.h"

// A location in an input source file.

// Used to replace Location default constructor
#define UNDEF_LOCATION UNKNOWN_LOCATION

#endif // !defined(RUST_LOCATION_H)
