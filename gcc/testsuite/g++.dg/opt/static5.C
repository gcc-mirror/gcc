// PR c++/31809
// { dg-do run }
// { dg-options "-O2" }

struct S
{
  unsigned v;
  static inline S f (unsigned a);
};

inline S
S::f (unsigned a)
{
  static S t = { a };
  return t;
}

const static S s = S::f (26);

extern "C" void abort (void);

int
main ()
{
  S t = s;
  if (t.v != 26)
    abort ();
  return 0;
}
