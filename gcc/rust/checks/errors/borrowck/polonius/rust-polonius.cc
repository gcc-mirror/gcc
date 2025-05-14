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

#include "rust-polonius.h"

namespace Rust {
namespace Polonius {

extern "C" {

FFI::FFIVector<size_t> *
FFIVector__new ()
{
  return FFI::FFIVector<size_t>::make_new ();
}

FFI::FFIVectorPair *
FFIVector__new_vec_pair ()
{
  return FFI::FFIVectorPair::make_new ();
}

FFI::FFIVectorTriple *
FFIVector__new_vec_triple ()
{
  return FFI::FFIVectorTriple::make_new ();
}

void
FFIVector__push (FFI::FFIVector<size_t> *vector, size_t element)
{
  vector->push (element);
}

void
FFIVector__push_vec_pair (FFI::FFIVectorPair *vector,
			  FFI::Pair<size_t, FFI::FFIVector<size_t> *> element)
{
  vector->push (element);
}

void
FFIVector__push_vec_triple (FFI::FFIVectorTriple *vector,
			    FFI::Triple<size_t, size_t, size_t> element)
{
  vector->push (element);
}
}

} // namespace Polonius
} // namespace Rust
