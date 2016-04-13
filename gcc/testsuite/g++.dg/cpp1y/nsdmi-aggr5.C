// PR c++/70332
// { dg-do run { target c++14 } }

template <class T>
struct C
{
 T m;
 T *n = &m;
};

C<int> c { };

int
main ()
{
  *c.n = 5;
  if (c.m != 5)
    __builtin_abort ();

  C<int> d { 10 };
  *d.n = *d.n + 1;
  if (d.m != 11)
    __builtin_abort ();
}
