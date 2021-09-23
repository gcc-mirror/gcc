/* { dg-do run { target ia32 } } */
/* { dg-options "-O2 -march=i686" } */

struct S { double _M_fp; };
union U { double d; unsigned long long int l; };

void
__attribute__((noipa))
foo (void)
{
  struct S a0, a1;
  union U u;
  double d0, d1;
  a0._M_fp = 0.0;
  a1._M_fp = 1.0;
  __atomic_store_8 (&a0._M_fp, __atomic_load_8 (&a1._M_fp, __ATOMIC_SEQ_CST), __ATOMIC_SEQ_CST);
  u.l = __atomic_load_8 (&a0._M_fp, __ATOMIC_SEQ_CST);
  d0 = u.d;
  u.l = __atomic_load_8 (&a1._M_fp, __ATOMIC_SEQ_CST);
  d1 = u.d;
  if (d0 != d1)
    __builtin_abort ();
}

int
main ()
{
  foo ();
  return 0;
}
