/* { dg-do compile } */
/* { dg-require-effective-target section_anchors } */
/* { dg-require-effective-target vect_int } */
/* { dg-options "-O2 -ftree-vectorize -fsection-anchors -fno-vect-cost-model -fdump-ipa-increase_alignment" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 32

static int a[N][N];
static int b[N][N];
static int c[N][N];

void clobber(int *);

int *foo(void)
{
  int i;
  int j;

  clobber (&a[0][0]);
  clobber (&b[0][0]);
  clobber (&c[0][0]);

  for (i = 0; i < N; i++) {
      for (j = 0; j < N; j++) {
	  c[j][i] += a[j][i] + c[j][i];
      }
  }
  return &c[0][0];
}

/* { dg-final { scan-ipa-dump-times "Increasing alignment of decl" 3 "increase_alignment" } } */
/* { dg -finalfoo { cleanup-ipa-dump "increase_alignment" } } */
