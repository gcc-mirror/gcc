/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a+nolse -mno-outline-atomics" } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=*" } { "" } } */

int
foo (int *a)
{
  int x = 0;
  return __atomic_compare_exchange_n (a, &x, 4, 0,
				      __ATOMIC_ACQUIRE, __ATOMIC_ACQUIRE);
}

/* { dg-final { scan-assembler-times "cbnz\\tw\[0-9\]+" 2 } } */
