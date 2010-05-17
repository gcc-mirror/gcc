// PR middle-end/44102
// { dg-do compile }
// { dg-options "-O2" }

void baz (void);
struct A { A (); ~A (); };

static inline int
foo (void)
{
  asm goto ("" : : : : l1, l2);
  __builtin_unreachable ();
 l1:
  return 1;
 l2:
  return 0;
}

int
bar (int x)
{
  if (x == 5)
    {
      A a, b;
      baz ();
    }
  if (foo () || x == 6)
    x = 1;
  else
    x = 2;
  return x;
}
