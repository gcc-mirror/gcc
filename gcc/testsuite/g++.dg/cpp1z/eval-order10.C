// PR c++/82959
// { dg-do run }
// { dg-additional-options -fstrong-eval-order }

struct A {};

void operator && (const A x, const A) {}
void operator || (const A x, const A) {}
void operator , (const A x, const A) {}

int i;

A f () { if (i != 0) __builtin_abort (); i = 1; return A (); }
A g () { if (i != 1) __builtin_abort (); i = 2; return A (); }

int
main ()
{
  f () && g ();
  if (i != 2) __builtin_abort ();
  i = 0;
  f () || g ();
  if (i != 2) __builtin_abort ();
  i = 0;
  f (), g ();
  if (i != 2) __builtin_abort ();
}
