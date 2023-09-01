// PR c++/111069
// { dg-do compile { target c++11 } }
// { dg-options "" }

extern int a[2];
struct Y { int b, c, d; };

inline int
freddy ()
{
  static auto [i, j] = a;		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
					// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
  static auto [k, l] = a;		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
					// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
  int ret = ++i + ++k;
  {
    static auto [i, j] = a;		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
					// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
    static auto [k, l] = a;		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
					// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
    ret += ++i + ++k;
  }
  {
    static auto [i, j] = a;		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
					// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
    static auto [k, l] = a;		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
					// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
    ret += ++i + ++k;
  }
  return ret;
}

namespace N
{
  namespace M
  {
    template <int N>
    inline int
    corge ()
    {
      static auto [i, j] = a;		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
					// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
      static auto && [u, v, w] = Y{};	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
					// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
      int ret = ++i + ++u;
      {
	static auto && [u, v, w] = Y{};	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
					// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
	ret += ++v;
      }
      return ret;
    }
  }
}

int (*p) () = &freddy;
int (*q) () = N::M::corge<3>;

// { dg-final { scan-assembler "_ZZ6freddyvEDC1i1jE" } }
// { dg-final { scan-assembler "_ZZ6freddyvEDC1i1jE_0" } }
// { dg-final { scan-assembler "_ZZ6freddyvEDC1i1jE_1" } }
// { dg-final { scan-assembler "_ZZ6freddyvEDC1k1lE" } }
// { dg-final { scan-assembler "_ZZ6freddyvEDC1k1lE_0" } }
// { dg-final { scan-assembler "_ZZ6freddyvEDC1k1lE_1" } }
// { dg-final { scan-assembler "_ZZN1N1M5corgeILi3EEEivEDC1i1jE" } }
// { dg-final { scan-assembler "_ZGVZ6freddyvEDC1i1jE" } }
// { dg-final { scan-assembler "_ZGVZ6freddyvEDC1i1jE_0" } }
// { dg-final { scan-assembler "_ZGVZ6freddyvEDC1i1jE_1" } }
// { dg-final { scan-assembler "_ZGVZ6freddyvEDC1k1lE" } }
// { dg-final { scan-assembler "_ZGVZ6freddyvEDC1k1lE_0" } }
// { dg-final { scan-assembler "_ZGVZ6freddyvEDC1k1lE_1" } }
// { dg-final { scan-assembler "_ZGVZN1N1M5corgeILi3EEEivEDC1i1jE" } }
// { dg-final { scan-assembler "_ZGRZN1N1M5corgeILi3EEEivEDC1u1v1wE_" } }
// { dg-final { scan-assembler "_ZGRZN1N1M5corgeILi3EEEivEDC1u1v1wE_0_" } }
