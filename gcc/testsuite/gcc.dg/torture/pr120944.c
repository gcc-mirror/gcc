/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
/* { dg-additional-options "-fdump-tree-optimized" } */

#include <stdlib.h>

typedef union {
  int u32;
  struct
  {
     int A:1;
     int B:2;
     int C:3;
  };
} u_t;

typedef union {
   volatile int u[3];
   volatile struct {
        u_t a;
        int b;
        int c;
   };
} DATA;

void foo (volatile DATA *d)
{
     d->a.u32 = ~0;
     u_t u = d->a;
     int v = u.A;
     if (v)
        abort();
}

/* { dg-final { scan-tree-dump-times "if \\\(" 1 "optimized" } } */
