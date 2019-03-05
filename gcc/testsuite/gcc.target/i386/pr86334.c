/* PR rtl-optimization/86334 */
/* { dg-do run { target ia32 } } */
/* { dg-options "-O -march=i386 -mtune=athlon -minline-all-stringops -minline-stringops-dynamically -mmemcpy-strategy=libcall:-1:align -Wno-psabi" } */

typedef int V __attribute__ ((vector_size (64)));

static inline V
foo (V g)
{
  g[0] = 4;
  return g;
}

int
main ()
{
  V x = foo ((V) { });
  if (x[0] != 4 || x[1] || x[2] || x[3] || x[4] || x[5] || x[6] || x[7])
    __builtin_abort ();
  return 0;
}
