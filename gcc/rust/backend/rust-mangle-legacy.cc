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

#include "rust-mangle.h"
#include "fnv-hash.h"
#include "rust-unicode.h"
#include "rust-diagnostics.h"
#include "rust-system.h"
#include <sstream>

namespace Rust {
namespace Compile {

const std::string kLegacySymbolPrefix = "_ZN";
static const std::string kLegacySymbolDelim = "E";
static const std::string kLegacyGenericDelim = "$C$";
static const std::string kLegacySubstBegin = "$LT$";
static const std::string kLegacySubstEnd = "$GT$";
static const std::string kLegacySpace = "$u20$";
static const std::string kLegacyRef = "$RF$";
static const std::string kLegacyPtr = "$BP$";
static const std::string kLegacyLeftSqParen = "$u5b$";	// [
static const std::string kLegacyRightSqParen = "$u5d$"; // ]
static const std::string kLegacyLeftBrace = "$u7b$";	// {
static const std::string kLegacyRightBrace = "$u7d$";	// }
static const std::string kQualPathBegin = "_" + kLegacySubstBegin;
static const std::string kLegacyComma = "$C$";

static std::string
legacy_mangle_name (const std::string &name)
{
  // example
  //  <&T as core::fmt::Debug>::fmt:
  //  _ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h6dac924c0051eef7E
  // replace all white space with $ and & with RF
  //
  // <example::Bar as example::A>::fooA:
  // _ZN43_$LT$example..Bar$u20$as$u20$example..A$GT$4fooA17hfc615fa76c7db7a0E:
  //
  // core::ptr::const_ptr::<impl *const T>::cast:
  // _ZN4core3ptr9const_ptr33_$LT$impl$u20$$BP$const$u20$T$GT$4cast17hb79f4617226f1d55E:
  //
  // core::ptr::const_ptr::<impl *const [T]>::as_ptr:
  // _ZN4core3ptr9const_ptr43_$LT$impl$u20$$BP$const$u20$$u5b$T$u5d$$GT$6as_ptr17he16e0dcd9473b04fE:
  //
  // example::Foo<T>::new:
  // _ZN7example12Foo$LT$T$GT$3new17h9a2aacb7fd783515E:
  //
  // <example::Identity as example::FnLike<&T,&T>>::call
  // _ZN74_$LT$example..Identity$u20$as$u20$example..FnLike$LT$$RF$T$C$$RF$T$GT$$GT$4call17ha9ee58935895acb3E

  tl::optional<Utf8String> utf8_name = Utf8String::make_utf8_string (name);
  rust_assert (utf8_name.has_value ());
  std::vector<Codepoint> chars = utf8_name.value ().get_chars ();
  std::string buffer;
  for (size_t i = 0; i < chars.size (); i++)
    {
      std::string m;
      Codepoint c = chars.at (i);

      if (c == ' ')
	m = kLegacySpace;
      else if (c == '&')
	m = kLegacyRef;
      else if (i == 0 && c == '<')
	m = kQualPathBegin;
      else if (c == '<')
	m = kLegacySubstBegin;
      else if (c == '>')
	m = kLegacySubstEnd;
      else if (c == '*')
	m = kLegacyPtr;
      else if (c == '[')
	m = kLegacyLeftSqParen;
      else if (c == ']')
	m = kLegacyRightSqParen;
      else if (c == '{')
	m = kLegacyLeftBrace;
      else if (c == '}')
	m = kLegacyRightBrace;
      else if (c == ',')
	m = kLegacyComma;
      else if (c == ':')
	{
	  rust_assert (i + 1 < chars.size ());
	  rust_assert (chars.at (i + 1) == ':');
	  i++;
	  m = "..";
	}
      else if (c.is_ascii ())
	// ASCII
	m.push_back (c.value);
      else
	{
	  // Non-ASCII
	  std::stringstream escaped;
	  escaped << std::hex << "$u" << c.value << "$";
	  m += escaped.str ();
	}
      buffer += m;
    }

  return std::to_string (buffer.size ()) + buffer;
}

static std::string
legacy_mangle_canonical_path (const Resolver::CanonicalPath &path)
{
  std::string buffer;
  for (size_t i = 0; i < path.size (); i++)
    {
      auto &seg = path.get_seg_at (i);
      buffer += legacy_mangle_name (seg.second);
    }
  return buffer;
}

// rustc uses a sip128 hash for legacy mangling, but an fnv 128 was quicker to
// implement for now
static std::string
legacy_hash (const std::string &fingerprint)
{
  Hash::FNV128 hasher;
  hasher.write ((const unsigned char *) fingerprint.c_str (),
		fingerprint.size ());

  uint64_t hi, lo;
  hasher.sum (&hi, &lo);

  char hex[16 + 1];
  memset (hex, 0, sizeof hex);
  snprintf (hex, sizeof hex, "%08" PRIx64 "%08" PRIx64, lo, hi);

  return "h" + std::string (hex, sizeof (hex) - 1);
}

std::string
legacy_mangle_item (const TyTy::BaseType *ty,
		    const Resolver::CanonicalPath &path)
{
  const std::string hash = legacy_hash (ty->mangle_string ());
  const std::string hash_sig = legacy_mangle_name (hash);

  return kLegacySymbolPrefix + legacy_mangle_canonical_path (path) + hash_sig
	 + kLegacySymbolDelim;
}

} // namespace Compile
} // namespace Rust
