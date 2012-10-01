/* PR54087.  Verify __atomic_sub (val) uses __atomic_add (-val) if there is no
             atomic_aub.  */
/* { dg-require-effective-target sync_int_long } */
/* { dg-do compile { target { i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-assembler-times "xadd" 2 } } */


int a;

int f1(int p)
{
  return __atomic_sub_fetch(&a, p, __ATOMIC_SEQ_CST) == 0;
}

int f2(int p)
{
  return __atomic_fetch_sub(&a, p, __ATOMIC_SEQ_CST) - p == 0;
}
