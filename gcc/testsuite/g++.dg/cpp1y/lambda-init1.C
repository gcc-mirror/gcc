// N3648: capture init
// { dg-options "-std=c++1y -w" }
// { dg-do run }

int main()
{
  int x = 41;
  auto r = [x = x+1]{ return x; }();
  if (r != 42) __builtin_abort();

  static auto *p = &r;
  [&x=r]{ if (&x != p) __builtin_abort(); }();
}
