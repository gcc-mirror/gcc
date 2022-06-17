// PR c++/105634
// { dg-do compile { target { c++11 } } }
// { dg-options "-Wall" }

struct s
{
  struct {} a[] = 1.0; // { dg-error "" }
  void f (char *c)
  {
    s s;
    __builtin_memcpy (&s, c, sizeof(s));
  }
};
