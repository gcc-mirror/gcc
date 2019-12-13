// { dg-do compile { target c++17 } }

void f(int i)
{
  [i]() constexpr {
    int j;			// { dg-error "uninitialized" "" { target c++17_down } }
    j = i;
    return j;
  }();
}
