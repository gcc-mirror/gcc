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

#ifndef RUST_INPUT_SOURCE_H
#define RUST_INPUT_SOURCE_H

#include "rust-codepoint.h"
#include "optional.h"

namespace Rust {

constexpr uint8_t UTF8_BOM1 = 0xEF;
constexpr uint8_t UTF8_BOM2 = 0xBB;
constexpr uint8_t UTF8_BOM3 = 0xBF;

// Input source wrapper thing.
class InputSource
{
private:
  // position of current character
  unsigned int pos;
  std::vector<Codepoint> chars;
  bool is_valid_utf8;

  // Overload operator () to return next char from input stream.
  virtual int next_byte () = 0;

  Codepoint next_codepoint ()
  {
    uint32_t input = next_byte ();

    if ((int32_t) input == EOF)
      return Codepoint::eof ();
    else if (input <= MAX_ASCII_CODEPOINT)
      {
	// ascii -- 1 byte
	return {input};
      }
    else if ((input & 0xC0) == 0x80)
      {
	// invalid (continuation; can't be first char)
	return {CODEPOINT_INVALID};
      }
    else if ((input & 0xE0) == 0xC0)
      {
	// 2 bytes
	uint8_t input2 = next_byte ();
	if ((input2 & 0xC0) != 0x80)
	  return {CODEPOINT_INVALID};

	uint32_t output = ((input & 0x1F) << 6) | ((input2 & 0x3F) << 0);
	return output;
      }
    else if ((input & 0xF0) == 0xE0)
      {
	// 3 bytes or UTF-8 BOM
	uint8_t input2 = next_byte ();
	// If the second byte is equal to 0xBB then the input is no longer a
	// valid UTF-8 char. Then, we check if the third byte makes up a UTF
	// BOM.
	if (input == UTF8_BOM1 && input2 == UTF8_BOM2)
	  {
	    uint8_t input3 = next_byte ();
	    if (input3 == UTF8_BOM3)
	      // found BOM
	      return next_codepoint ();
	    else
	      return {CODEPOINT_INVALID};
	  }

	if ((input2 & 0xC0) != 0x80)
	  return {CODEPOINT_INVALID};

	uint8_t input3 = next_byte ();

	if ((input3 & 0xC0) != 0x80)
	  return {CODEPOINT_INVALID};

	uint32_t output = ((input & 0x0F) << 12) | ((input2 & 0x3F) << 6)
			  | ((input3 & 0x3F) << 0);
	return {output};
      }
    else if ((input & 0xF8) == 0xF0)
      {
	// 4 bytes
	uint8_t input2 = next_byte ();
	if ((input2 & 0xC0) != 0x80)
	  return {CODEPOINT_INVALID};

	uint8_t input3 = next_byte ();
	if ((input3 & 0xC0) != 0x80)
	  return {CODEPOINT_INVALID};

	uint8_t input4 = next_byte ();
	if ((input4 & 0xC0) != 0x80)
	  return {CODEPOINT_INVALID};

	uint32_t output = ((input & 0x07) << 18) | ((input2 & 0x3F) << 12)
			  | ((input3 & 0x3F) << 6) | ((input4 & 0x3F) << 0);
	return {output};
      }
    else
      {
	return {CODEPOINT_INVALID};
      }
  }

protected:
  // This method must be called by the constructor to initialize the input
  // source. We cannot move this to the constructor because it calls a
  // virtual method .
  void init ()
  {
    // Check if the input source is valid as utf-8 and copy all characters to
    // `chars`.
    Codepoint char32 = next_codepoint ();
    while (!char32.is_eof () && char32 != CODEPOINT_INVALID)
      {
	chars.push_back (char32);
	char32 = next_codepoint ();
      }

    if (char32 == CODEPOINT_INVALID)
      {
	// Input source is not valid as utf-8.
	is_valid_utf8 = false;
      }
  }

public:
  InputSource () : pos (0), chars ({}), is_valid_utf8 (true) {}

  virtual ~InputSource () {}

  // Checks if input source is a valid UTF-8 string
  bool is_valid () { return is_valid_utf8; }

  // get the next UTF-8 character
  Codepoint next ()
  {
    if (pos >= chars.size ())
      return Codepoint::eof ();
    else
      {
	Codepoint c = chars[pos];
	pos++;
	return c;
      }
  }

  // Returns codepoint if input source is a valid UTF-8 string. Returns
  // nullopt otherwise.
  tl::optional<std::vector<Codepoint>> get_chars ()
  {
    if (is_valid ())
      return {chars};
    else
      return tl::nullopt;
  }
};

class FileInputSource : public InputSource
{
private:
  // Input source file.
  FILE *input;

  int next_byte () override { return fgetc (input); }

public:
  // Create new input source from file.
  FileInputSource (FILE *input) : InputSource (), input (input) { init (); }
};

class BufferInputSource : public InputSource
{
private:
  const std::string &buffer;
  size_t offs;

  int next_byte () override
  {
    if (offs >= buffer.size ())
      return EOF;
    return static_cast<uint8_t> (buffer.at (offs++));
  }

public:
  // Create new input source from file.
  BufferInputSource (const std::string &b, size_t offset)
    : InputSource (), buffer (b), offs (offset)
  {
    init ();
  }
};

} // namespace Rust

#endif
