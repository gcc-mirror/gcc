// PR debug/104407
// { dg-do compile { target c++17 } }
// { dg-options "-O1 -fcompare-debug" }

struct A { int i; long j; int k : 2; char l; } a;

auto [ aa, bb, cc, dd ] = a;

namespace N
{
  auto & [ m, n, o, ppp ] = a;
}
