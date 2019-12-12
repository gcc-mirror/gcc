/* PR target/88906 */
/* { dg-do run { target ia32 } } */
/* { dg-options "-O -march=i386 -mtune=k6 -minline-all-stringops -minline-stringops-dynamically -mmemcpy-strategy=libcall:-1:align -Wno-psabi" } */

typedef unsigned V __attribute__ ((vector_size (16)));

static inline V
foo (V v)
{
  __builtin_sub_overflow (0, 0, &v[0]);
  return v;
}

int
main ()
{
  V v = foo ((V) { ~0 });
  if (v[0] || v[1] || v[2] || v[3])
    __builtin_abort ();
  return 0;
}
