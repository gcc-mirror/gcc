// { dg-do compile { target c++11 } }
// { dg-options "" }

struct A { int i; long j; int k : 2; char l; } a;
int c[2];
struct B { template<int I> int &get () { return c[I]; } } b;
namespace std {
  template<typename T> struct tuple_size;
  template<int, typename> struct tuple_element;
}
template<> struct std::tuple_size<B> { static constexpr int value = 2; };
template<int I> struct std::tuple_element<I,B> { typedef int type; };

auto [ aa, bb, cc, dd ] = a;		// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
// { dg-final { scan-assembler "_ZDC2aa2bb2cc2ddE" } }
const auto & [ e, f, g, h ] = a;	// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
// { dg-final { scan-assembler "_ZDC1e1f1g1hE" } }
auto [ ee, ff ] = b;			// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
// { dg-final { scan-assembler "_ZDC2ee2ffE" } }
auto & [ gg, hh ] = b;			// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
// { dg-final { scan-assembler "_ZDC2gg2hhE" } }
namespace N
{
  namespace M
  {
    auto [ i, j, k, l ] = a;		// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
    // { dg-final { scan-assembler "_ZN1N1MDC1i1j1k1lEE" } }
    auto & [ m, n, o, ppp ] = a;	// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
    // { dg-final { scan-assembler "_ZN1N1MDC1m1n1o3pppEE" } }
    auto [ ii, jj ] = b;		// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
    // { dg-final { scan-assembler "_ZN1N1MDC2ii2jjEE" } }
    // { dg-final { scan-assembler "_ZN1N1M2iiE" } }
    // { dg-final { scan-assembler "_ZN1N1M2jjE" } }
    auto & [ mm, nn ] = b;		// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
    // { dg-final { scan-assembler "_ZN1N1MDC2mm2nnEE" } }
    // { dg-final { scan-assembler "_ZN1N1M2mmE" } }
    // { dg-final { scan-assembler "_ZN1N1M2nnE" } }
  }
}
namespace std
{
  auto [ i2, j2, k2, l2 ] = a;		// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
  // { dg-final { scan-assembler "_ZStDC2i22j22k22l2E" } }
  auto [ vv, ww ] = b;			// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
  // { dg-final { scan-assembler "_ZStDC2vv2wwE" } }
  // { dg-final { scan-assembler "_ZSt2vv" } }
  // { dg-final { scan-assembler "_ZSt2ww" } }
}
namespace
{
  auto [ v, w, x, y ] = a;		// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
}
