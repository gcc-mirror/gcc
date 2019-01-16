// PR c++/87476
// { dg-do compile { target c++11 } }

void f0 () { constexpr char p[] = "11111"; }
void f1 () { constexpr unsigned char p[] = "11111"; }
void f2 () { constexpr signed char p[] = "11111"; }
template <int N>
void f3 () { constexpr char p[] = "11111"; }
template <int N>
void f4 () { constexpr unsigned char p[] = "11111"; }
template <int N>
void f5 () { constexpr signed char p[] = "11111"; }

void
baz ()
{
  f0 ();
  f1 ();
  f2 ();
  f3<0> ();
  f4<0> ();
  f5<0> ();
}
