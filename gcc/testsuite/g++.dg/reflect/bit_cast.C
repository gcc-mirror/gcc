// PR c++/124096
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <bit>

consteval void
f ()
{
  auto a = ^^int;
  auto *b = &a;
  void *c = nullptr;
  (void) std::bit_cast<void *>(b);
  (void) std::bit_cast<decltype(^^int) *>(c);
  __builtin_bit_cast (void *, b);
  __builtin_bit_cast (decltype(^^int) *, c);
}

void
g ()
{
  constexpr static auto a = ^^int;
  constexpr auto *b = &a;
  void *c = nullptr;
  (void) std::bit_cast<void *>(b);		// { dg-error "consteval-only value outside an immediate function context" }
  (void) std::bit_cast<decltype(^^int) *>(c);
  __builtin_bit_cast (void *, b);		// { dg-error "consteval-only value outside an immediate function context" }
  __builtin_bit_cast (decltype(^^int) *, c);
}
