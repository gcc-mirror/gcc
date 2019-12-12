/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a+nolse -mno-outline-atomics" } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=*" } { "" } } */

int
foo (int *a)
{
  int x = 3;
  return __atomic_compare_exchange_n (a, &x, 0, 1, __ATOMIC_ACQUIRE, __ATOMIC_ACQUIRE);
}

/* { dg-final { scan-assembler "stxr\\tw\[0-9\]+, wzr,.*" } } */
/* { dg-final { scan-assembler-not "mov\\tw\[0-9\]+, 0" } } */
