// PR c++/124496
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

template<unsigned>
struct A {};

template<int N>
static constexpr int var = N;

template<decltype(^^void) R, typename T>
constexpr auto f ()   // { dg-error "deduced class type .A. in function return type" }
  -> A<([:R<T>:])>    // { dg-error "invalid|expected" }
{
  return {};
}

constexpr auto a = f<^^var, int>();   // { dg-error "declared" }
