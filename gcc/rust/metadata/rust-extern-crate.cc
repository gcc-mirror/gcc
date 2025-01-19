// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

#include "rust-extern-crate.h"
#include "rust-diagnostics.h"
#include "rust-export-metadata.h"

#include "md5.h"

namespace Rust {
namespace Imports {

ExternCrate::ExternCrate (Import::Stream &stream) : import_stream (stream) {}

ExternCrate::ExternCrate (const std::string &crate_name,
			  std::vector<ProcMacro::Procmacro> macros)
  : proc_macros (macros), crate_name (crate_name)
{}

ExternCrate::~ExternCrate () {}

bool
ExternCrate::ok () const
{
  return !import_stream->get ().saw_error ();
}

bool
ExternCrate::load (location_t locus)
{
  rust_assert (this->import_stream.has_value ());
  auto &import_stream = this->import_stream->get ();
  // match header
  import_stream.require_bytes (locus, Metadata::kMagicHeader,
			       sizeof (Metadata::kMagicHeader));
  if (import_stream.saw_error ())
    return false;

  // parse 16 bytes md5
  unsigned char checksum[16];
  bool ok
    = import_stream.do_peek (sizeof (checksum), (const char **) &checksum);
  if (!ok)
    return false;

  import_stream.advance (sizeof (checksum));

  // parse delim
  import_stream.require_bytes (locus, Metadata::kSzDelim,
			       sizeof (Metadata::kSzDelim));
  if (import_stream.saw_error ())
    return false;

  // parse crate name
  bool saw_delim = false;
  while (!import_stream.saw_error () && !import_stream.at_eof ())
    {
      unsigned char byte = import_stream.get_char ();
      saw_delim
	= memcmp (&byte, Metadata::kSzDelim, sizeof (Metadata::kSzDelim)) == 0;
      if (saw_delim)
	break;

      crate_name += byte;
    }
  if (!saw_delim || crate_name.empty ())
    {
      import_stream.set_saw_error ();
      rust_error_at (locus, "failed to read crate name field");

      return false;
    }

  // read until delim which is the size of the meta data
  std::string metadata_length_buffer;
  saw_delim = false;
  while (!import_stream.saw_error () && !import_stream.at_eof ())
    {
      unsigned char byte = import_stream.get_char ();
      saw_delim
	= memcmp (&byte, Metadata::kSzDelim, sizeof (Metadata::kSzDelim)) == 0;
      if (saw_delim)
	break;

      metadata_length_buffer += byte;
    }
  if (!saw_delim || metadata_length_buffer.empty ())
    {
      import_stream.set_saw_error ();
      rust_error_at (locus, "failed to read metatadata size");

      return false;
    }

  // interpret the string size
  int expected_buffer_length = -1;
  ok = ExternCrate::string_to_int (locus, metadata_length_buffer, false,
				   &expected_buffer_length);
  if (!ok)
    return false;

  // read the parsed size and it should be eof
  metadata_buffer.reserve (expected_buffer_length);
  for (int i = 0; i < expected_buffer_length && !import_stream.saw_error ()
		  && !import_stream.at_eof ();
       i++)
    {
      metadata_buffer += import_stream.get_char ();
    }

  // compute the md5
  struct md5_ctx chksm;
  unsigned char computed_checksum[16];

  md5_init_ctx (&chksm);
  md5_process_bytes (metadata_buffer.c_str (), metadata_buffer.size (), &chksm);
  md5_finish_ctx (&chksm, computed_checksum);

  // FIXME i think the encoding and decoding of md5 is going wrong or else we
  // are not computing it correctly
  //
  // compare the checksums
  // if (memcmp(computed_checksum, checksum, sizeof (checksum)) != 0)
  //   {
  //     rust_error_at (locus,
  //       	     "checksum mismatch in metadata: %<%.*s%> vs %<%.*s%>",
  //       	     sizeof (computed_checksum), computed_checksum,
  //       	     sizeof (checksum), checksum);
  //     return false;
  //   }

  // all good
  return true;
}

const std::string &
ExternCrate::get_crate_name () const
{
  return crate_name;
}

const std::string &
ExternCrate::get_metadata () const
{
  return metadata_buffer;
}

// Turn a string into a integer with appropriate error handling.
bool
ExternCrate::string_to_int (location_t locus, const std::string &s,
			    bool is_neg_ok, int *ret)
{
  char *end;
  long prio = strtol (s.c_str (), &end, 10);
  if (*end != '\0' || prio > 0x7fffffff || (prio < 0 && !is_neg_ok))
    {
      rust_error_at (locus, "invalid integer in import data");
      return false;
    }
  *ret = prio;
  return true;
}

} // namespace Imports
} // namespace Rust
