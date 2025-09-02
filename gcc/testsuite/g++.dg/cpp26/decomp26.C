// P1061R10 - Structured Bindings can introduce a Pack
// { dg-do compile { target c++11 } }
// { dg-options "-fno-implicit-constexpr" }
// { dg-final { scan-assembler "_ZZ3fooI1AEivE1a:" } }
// { dg-final { scan-assembler "_ZZ3fooI1AEivE1b:" } }
// { dg-final { scan-assembler "_ZZ3fooI1AEivE1c:" } }
// { dg-final { scan-assembler "_ZZ3fooI1AEivE1a_0:" } }
// { dg-final { scan-assembler "_ZZ3fooI1AEivE1b_0:" } }
// { dg-final { scan-assembler "_ZZ3fooI1AEivE1c_0:" } }
// { dg-final { scan-assembler "_ZZ3fooI1AEivEDC1a1b1cE:" } }
// { dg-final { scan-assembler "_ZZ3fooI1AEivE1a_1:" } }
// { dg-final { scan-assembler "_ZZ3fooI1AEivE1b_1:" } }
// { dg-final { scan-assembler "_ZZ3fooI1AEivE1c_1:" } }
// { dg-final { scan-assembler "_ZZ3fooI1BEivE1a:" } }
// { dg-final { scan-assembler "_ZZ3fooI1BEivE1b:" } }
// { dg-final { scan-assembler "_ZZ3fooI1BEivE1c:" } }
// { dg-final { scan-assembler "_ZZ3fooI1BEivE1a_0:" } }
// { dg-final { scan-assembler "_ZZ3fooI1BEivE1b_0:" } }
// { dg-final { scan-assembler "_ZZ3fooI1BEivE1c_0:" } }
// { dg-final { scan-assembler "_ZZ3fooI1BEivEDC1a1b1cE:" } }
// { dg-final { scan-assembler "_ZZ3fooI1BEivE1a_1:" } }
// { dg-final { scan-assembler "_ZZ3fooI1BEivE1b_1:" } }
// { dg-final { scan-assembler "_ZZ3fooI1BEivE1b_2:" } }
// { dg-final { scan-assembler "_ZZ3fooI1BEivE1b_3:" } }
// { dg-final { scan-assembler "_ZZ3fooI1BEivE1c_1:" } }
// { dg-final { scan-assembler "_ZZ3fooI1BEivE1a_2:" } }
// { dg-final { scan-assembler "_ZZ3fooI1BEivE1b_4:" } }
// { dg-final { scan-assembler "_ZZ3fooI1BEivE1c_2:" } }

template <typename T>
int
foo ()
{
  static int a = 1, b = 2, c = 3;
  int d = a++ + b++ + c++;
  {
    static int a = 1, b = 2, c = 3;
    d += a++ + b++ + c++;
    {
      static auto [a, ...b, c] = T {};	// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-2 }
      d += a++ + b...[0]++ + c++;	// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      {
	static int a = 1, b = 2, c = 3;
	return d + a++ + b++ + c++;
      }
    }
  }
}

struct A { int a, b, c, d, e; };

void
bar ()
{
  foo <A> ();
}

namespace std {
  template<typename T> struct tuple_size;
  template<int, typename> struct tuple_element;
}

struct B {
  int a[5];
  template <int I> int &get () { return a[I]; }
};
         
template<> struct std::tuple_size<B> { static const int value = 5; };
template<int I> struct std::tuple_element<I,B> { using type = int; };

void
baz ()
{
  foo <B> ();
}
