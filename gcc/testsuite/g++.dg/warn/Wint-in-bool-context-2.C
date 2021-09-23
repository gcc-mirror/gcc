// { dg-do compile { target c++11 } }
// { dg-options "-Wint-in-bool-context" }

unsigned hb(unsigned i) { return ~i; }

template<typename T>
void f(int i)
{
  auto l = [i]() { return T(2 * hb(i)); }; // { dg-bogus "in boolean context" }
  (void) l;
}

int main()
{
  f<bool>(0);
}
