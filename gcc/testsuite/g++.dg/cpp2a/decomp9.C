// PR c++/111069
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct [[gnu::abi_tag ("foobar")]] S { int i; };
extern S a[2];
struct [[gnu::abi_tag ("qux")]] T { int i; S j; int k; };
extern T b[2];

namespace N {
  auto [i, j] = a;		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  auto [k, l] = b;		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
}

inline int
foo ()
{
  static auto [m, n] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
				// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
  static auto [o, p] = b;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
				// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
  int ret = ++N::i.i + ++N::k.i;
  {
    static auto [m, n] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
				// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }

    ret += ++n.i;
  }
  {
    static auto [m, n] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
				// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }

    ret += ++n.i;
  }
  ret += ++m.i + ++o.i;
  return ret;
}

template <typename T>
inline int
bar ()
{
  static auto [m, n] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
				// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
  static auto [o, p] = b;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
				// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
  int ret = 0;
  {
    static auto [m, n] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
				// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
    ret += ++n.i;
  }
  {
    static auto [m, n] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
				// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
    ret += ++n.i;
  }
  ret += ++m.i + ++o.i;
  return ret;
}

int (*p) () = &foo;
int (*q) () = &bar<T>;

// { dg-final { scan-assembler "_ZZ3foovEDC1m1nEB6foobar" } }
// { dg-final { scan-assembler "_ZZ3foovEDC1m1nEB6foobar_0" } }
// { dg-final { scan-assembler "_ZZ3foovEDC1m1nEB6foobar_1" } }
// { dg-final { scan-assembler "_ZZ3foovEDC1o1pEB3qux" } }
// { dg-final { scan-assembler "_ZZ3barI1TB3quxEivEDC1m1nEB6foobar" } }
// { dg-final { scan-assembler "_ZZ3barI1TB3quxEivEDC1m1nEB6foobar_0" } }
// { dg-final { scan-assembler "_ZZ3barI1TB3quxEivEDC1m1nEB6foobar_1" } }
// { dg-final { scan-assembler "_ZZ3barI1TB3quxEivEDC1o1pEB3qux" } }
// { dg-final { scan-assembler "_ZN1NDC1i1jEB6foobarE" } }
// { dg-final { scan-assembler "_ZN1NDC1k1lEB3quxE" } }
// { dg-final { scan-assembler "_ZGVZ3foovEDC1m1nEB6foobar" } }
// { dg-final { scan-assembler "_ZGVZ3foovEDC1m1nEB6foobar_0" } }
// { dg-final { scan-assembler "_ZGVZ3foovEDC1m1nEB6foobar_1" } }
// { dg-final { scan-assembler "_ZGVZ3foovEDC1o1pEB3qux" } }
// { dg-final { scan-assembler "_ZGVZ3barI1TB3quxEivEDC1m1nEB6foobar" } }
// { dg-final { scan-assembler "_ZGVZ3barI1TB3quxEivEDC1m1nEB6foobar_0" } }
// { dg-final { scan-assembler "_ZGVZ3barI1TB3quxEivEDC1m1nEB6foobar_1" } }
// { dg-final { scan-assembler "_ZGVZ3barI1TB3quxEivEDC1o1pEB3qux" } }
