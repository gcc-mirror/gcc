// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection -fno-exceptions" }

#include <meta>

consteval bool
foo ()
{
  auto ctx = std::meta::access_context::unchecked ();
  auto ctx2 = ctx.via (^^::);	// { dg-message "in 'constexpr' expansion of 'ctx.std::meta::access_context::via\\\(\\\^\\\^::\\\)'" }
  return true;
}

auto b = foo ();		// { dg-message "in 'constexpr' expansion of 'foo\\\(\\\)'" }
// { dg-error "constexpr message: std::meta::access_context::via argument other than null or complete class type reflection" "" { target *-*-* } 0 }
