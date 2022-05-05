// PR c++/66139
// { dg-do run { target c++11 } }

#include <initializer_list>

int c, d;

struct a
{
  a (int i) { if (i) throw i; c++; }
  ~a () { d++; }
};

void check (void (*f) ())
{
  try
  {
    c = d = 0;
    f ();
  }
  catch (int)
  {
    if (c != 1 || d != 1)
      __builtin_abort ();
    return;
  }
  __builtin_abort ();
}

int main ()
{
  struct s { a x, y; };
  check ([] { s t { 0, 1 }; });
  check ([] { s { 0, 1 }; });
  check ([] { a t[2] { 0, 1 }; });
  using array = a[2];
  check ([] { array { 0, 1 }; });
  check ([] { std::initializer_list <a> t { 0, 1 }; });
  check ([] { std::initializer_list <a> { 0, 1 }; });
}
