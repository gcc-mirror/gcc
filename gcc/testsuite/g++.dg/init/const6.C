// PR c++/36688
// { dg-do run }
// { dg-options "-O2" }

struct S
{
  long long a;
  long long b;
  long long c;
};

struct T
{
  S g;
  long long h[12];
};

static const S s = { 1, 2, 3 };
static const T t = { s, 0 };

int
main ()
{
  T x = t;
  if (__builtin_memcmp (&x, &t, sizeof (T)))
    __builtin_abort ();
}
