// PR c++/125820
// P4101R1, Consteval-only Values for C++26
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^::);

struct S { info i; };

// These are non-constant under https://eel.is/c++draft/bit.cast#2 and
// https://eel.is/c++draft/expr.const#core-1.2
constexpr auto x = __builtin_bit_cast (unsigned long, ^^int);   // { dg-error ".__builtin_bit_cast. is not a constant expression because its type is .std::meta::info." }
constexpr auto y = __builtin_bit_cast (decltype(^^int), 0UL);   // { dg-error ".__builtin_bit_cast. is not a constant expression because its type is .std::meta::info." }
constexpr auto w = __builtin_bit_cast (S, 0UL);   // { dg-error ".__builtin_bit_cast. is not a constant expression because .S. contains .std::meta::info. type" }
// TODO: CWG 3217. Run-time treatment of std::meta::info types
info z = __builtin_bit_cast (decltype(^^int), 0xdeadbeefUL);
