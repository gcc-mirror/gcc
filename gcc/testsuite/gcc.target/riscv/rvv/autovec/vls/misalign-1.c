/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m4 -fno-tree-loop-distribute-patterns -mno-vector-strict-align -mstringop-strategy=libcall" } */

#include <stdlib.h>

typedef union U { unsigned short s; unsigned char c; } __attribute__((packed)) U;
struct S { char e __attribute__((aligned (64))); U s[32]; };
struct S t = {0, {{1}, {2}, {3}, {4}, {5}, {6}, {7}, {8},
		  {9}, {10}, {11}, {12}, {13}, {14}, {15}, {16},
		  {17}, {18}, {19}, {20}, {21}, {22}, {23}, {24},
		  {25}, {26}, {27}, {28}, {29}, {30}, {31}, {32}}};
unsigned short d[32] = { 1 };

__attribute__((noinline, noclone)) void
foo ()
{
  int i;
  for (i = 0; i < 32; i++)
    d[i] = t.s[i].s;
  if (__builtin_memcmp (d, t.s, sizeof d))
    abort ();
}

/* { dg-final { scan-assembler-times {vsetvli} 1 } } */

