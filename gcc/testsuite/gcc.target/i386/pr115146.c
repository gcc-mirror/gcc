/* { dg-do run { target sse2_runtime } } */
/* { dg-options "-O2 -msse2" } */

typedef unsigned char v8qi __attribute__((vector_size (8)));

v8qi res, a;

void __attribute__((noipa))
foo (void)
{
  res = __builtin_shufflevector(a, a, 1, 0, 3, 2, 5, 4, 7, 6);
}

void
comp (v8qi a, v8qi b, int n)
{
  for (unsigned i = 0; i < n; ++i)
    if ((a)[i] != (b)[i])
      __builtin_abort ();
}

#define E0 140
#define E1 141
#define E2 142
#define E3 143
#define E4 144
#define E5 145
#define E6 146
#define E7 147

int main()
{
  a = (v8qi) { E0, E1, E2, E3, E4, E5, E6, E7 };
  foo ();
  comp (res, ((v8qi) { E1, E0, E3, E2, E5, E4, E7, E6 }), 8);
  return 0;
}

