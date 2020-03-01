/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */

typedef struct bar {
  void *a;
  void *b;
} TYPE;

void foo (TYPE *p, TYPE *q) { *p = *q; }

/* { dg-final { scan-assembler-not "xxpermdi" } } */
