/* This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>. */

#ifndef RUST_CFG_PARSER_H
#define RUST_CFG_PARSER_H

#include "rust-system.h"

namespace Rust {
/**
 * Parse a `key` or `key="value"` pair given to the `-frust-cfg` compiler
 * option.
 *
 * The format is as follows:
 *
 * -frust-cfg=<input>
 *
 * cfg_input: identifier | identifier '=' '"' identifier '"'
 *
 * @param input User input given to the -frust-cfg option
 * @param key String in which to store the parsed `key`.
 * @param value String in which to store the parsed `value` if it exists
 *
 * @return false if the given input was invalid, true otherwise
 */
bool
parse_cfg_option (std::string &input, std::string &key, std::string &value);
} // namespace Rust

#if CHECKING_P

namespace selftest {
extern void
rust_cfg_parser_test (void);
} // namespace selftest

#endif // CHECKING_P

#endif // RUST_CFG_PARSER_H
