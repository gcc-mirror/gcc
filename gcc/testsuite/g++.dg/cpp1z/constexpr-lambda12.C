// { dg-do compile { target c++17 } }

void f(int i)
{
  [i]() constexpr {
    int j;			// { dg-error "uninitialized" }
    j = i;
    return j;
  }();
}
