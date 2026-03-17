// PR c++/124417
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection -fno-exceptions" }

#include <meta>

auto a = is_enum_type (^^::);
// { dg-error "call to consteval function 'std::meta::is_enum_type\\\(\\\^\\\^::\\\)' is not a constant expression" "" { target *-*-* } .-1 }
// { dg-error "'consteval bool std::meta::is_enum_type\\\(info\\\)' should throw 'std::meta::exception'; 'what\\\(\\\)': 'reflection does not represent a type'" "" { target *-*-* } .-2 }
// { dg-message "exceptions are disabled, treating as non-constant" "" { target *-*-* } .-3 }
