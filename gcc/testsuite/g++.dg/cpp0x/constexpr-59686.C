// PR c++/59686
// { dg-do compile { target c++11 } }

int main()
{
  static const int x = 5;
  const int * const y = &x;
  static_assert(y, "");  // { dg-error "non-constant|value" }
}
