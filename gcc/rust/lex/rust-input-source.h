#ifndef RUST_INPUT_SOURCE_H
#define RUST_INPUT_SOURCE_H

#include "rust-codepoint.h"
#include "optional.h"

namespace Rust {
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
    else if (input < 128)
      {
	// ascii -- 1 byte
	return {input};
      }
    else if ((input & 0xC0) == 0x80)
      {
	// invalid (continuation; can't be first char)
	return {0xFFFE};
      }
    else if ((input & 0xE0) == 0xC0)
      {
	// 2 bytes
	uint8_t input2 = next_byte ();
	if ((input2 & 0xC0) != 0x80)
	  return {0xFFFE};

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
	if (input == 0xEF && input2 == 0xBB)
	  {
	    uint8_t input3 = next_byte ();
	    if (input3 == 0xBF)
	      // found BOM
	      return next_codepoint ();
	    else
	      return {0xFFFE};
	  }

	if ((input2 & 0xC0) != 0x80)
	  return {0xFFFE};

	uint8_t input3 = next_byte ();

	if ((input3 & 0xC0) != 0x80)
	  return {0xFFFE};

	uint32_t output = ((input & 0x0F) << 12) | ((input2 & 0x3F) << 6)
			  | ((input3 & 0x3F) << 0);
	return {output};
      }
    else if ((input & 0xF8) == 0xF0)
      {
	// 4 bytes
	uint8_t input2 = next_byte ();
	if ((input2 & 0xC0) != 0x80)
	  return {0xFFFE};

	uint8_t input3 = next_byte ();
	if ((input3 & 0xC0) != 0x80)
	  return {0xFFFE};

	uint8_t input4 = next_byte ();
	if ((input4 & 0xC0) != 0x80)
	  return {0xFFFE};

	uint32_t output = ((input & 0x07) << 18) | ((input2 & 0x3F) << 12)
			  | ((input3 & 0x3F) << 6) | ((input4 & 0x3F) << 0);
	return {output};
      }
    else
      {
	return {0xFFFE};
      }
  }

protected:
  // Check if the input source is valid as utf-8 and copy all characters to
  // `chars`.
  void init ()
  {
    Codepoint char32 = next_codepoint ();
    while (!char32.is_eof () && char32 != 0xFFFE)
      {
	chars.push_back (char32);
	char32 = next_codepoint ();
      }

    if (char32 == 0xFFFE)
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
  FileInputSource (FILE *input) : InputSource (), input (input)
  {
    // TODO make this better?
    init ();
  }
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
    return (uint8_t) buffer.at (offs++);
  }

public:
  // Create new input source from file.
  BufferInputSource (const std::string &b, size_t offset)
    : InputSource (), buffer (b), offs (offset)
  {
    // TODO make this better?
    init ();
  }
};

} // namespace Rust

#endif
