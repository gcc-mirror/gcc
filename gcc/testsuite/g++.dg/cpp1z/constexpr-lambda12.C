// { dg-options -std=c++1z }

void f(int i)
{
  [i]() constexpr {
    int j;			// { dg-error "uninitialized" }
    j = i;
    return j;
  }();
}
