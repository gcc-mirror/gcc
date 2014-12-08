/* { dg-do compile } */
/* { dg-options "-std=c11 -gdwarf-5 -dA" } */

struct Atomics
{
  _Atomic(int) counter;
  struct Pointer
  {
    _Atomic volatile char *p;
  } p;
};

struct Atomics a;

/* { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_atomic_type" 2 } } */
