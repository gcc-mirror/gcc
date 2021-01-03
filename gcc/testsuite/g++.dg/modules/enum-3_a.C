// { dg-additional-options -fmodules-ts }
// from https://godbolt.org/beta/z/V45BSw

export module m0;
// { dg-module-cmi m0 }
namespace m0_ns
{
template <typename T> struct s0 {
  enum t { a };
};
}
