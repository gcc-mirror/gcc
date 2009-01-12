/* PR c/32041 */
/* { dg-do run } */

struct S
{
  int c;
  struct { float f; } sa[2];
};

char a[__builtin_offsetof (S, sa->f)
       == __builtin_offsetof (S, sa[0].f) ? 1 : -1];

template <int N>
struct T
{
  int c[N];
  struct { float f; } sa[N];
  static int foo () { return __builtin_offsetof (T, sa->f); }
  static int bar () { return __builtin_offsetof (T, sa[0].f); }
};

char b[__builtin_offsetof (T<5>, sa->f)
       == __builtin_offsetof (T<5>, sa[0].f) ? 1 : -1];

int
main ()
{
  if (T<1>::foo () != T<1>::bar ())
    __builtin_abort ();
  if (T<7>::foo () != T<7>::bar ())
    __builtin_abort ();
}
