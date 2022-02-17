#include "rust-cfg-parser.h"
#include "selftest.h"

namespace Rust {
bool
parse_cfg_option (const std::string &input, std::string &key,
		  std::string &value)
{
  key.clear ();
  value.clear ();

  auto equal = input.find ('=');

  // If there is no equal sign, it means there is no value. Clean up the key
  // and return
  if (equal == std::string::npos)
    {
      key = input;

      // FIXME: Make sure key is a proper identifier

      return true;
    }

  key = input.substr (0, equal);

  auto remaining_input = input.substr (equal + 1);
  if (remaining_input[0] != '"' || remaining_input.back () != '"')
    return false;

  // Remove the quotes around the value, by advancing one character
  value = remaining_input.substr (1);
  // And trimming the rightmost character. This is fine since we've already
  // checked that both the first and last characters were quotes.
  value.resize (value.size () - 1);

  // FIXME: We need to sanitize here and make sure that both key and value
  // are proper identifiers

  return true;
}

} // namespace Rust

#if CHECKING_P

namespace selftest {

void
rust_cfg_parser_test (void)
{
  std::string key;
  std::string value;

  ASSERT_TRUE (Rust::parse_cfg_option ("key-no-value", key, value));
  ASSERT_EQ (key, "key-no-value");
  ASSERT_TRUE (value.empty ());

  ASSERT_TRUE (Rust::parse_cfg_option ("k=\"v\"", key, value));
  ASSERT_EQ (key, "k");
  ASSERT_EQ (value, "v");

  // values should be between double quotes
  ASSERT_FALSE (Rust::parse_cfg_option ("k=v", key, value));

  // No value is an error if there is an equal sign
  ASSERT_FALSE (Rust::parse_cfg_option ("k=", key, value));

  // No key is an error
  ASSERT_FALSE (Rust::parse_cfg_option ("=", key, value));
  ASSERT_FALSE (Rust::parse_cfg_option ("=value", key, value));
}
} // namespace selftest

#endif // CHECKING_P
