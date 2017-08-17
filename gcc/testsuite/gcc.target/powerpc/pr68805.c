/* { dg-do compile { target powerpc64le-*-* } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-O2 -mcpu=power8" } */

typedef struct bar {
  void *a;
  void *b;
} TYPE;

void foo (TYPE *p, TYPE *q) { *p = *q; }

/* { dg-final { scan-assembler     "lxvd2x"   } } */
/* { dg-final { scan-assembler     "stxvd2x"  } } */
/* { dg-final { scan-assembler-not "xxpermdi" } } */

