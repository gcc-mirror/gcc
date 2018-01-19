/* { dg-do compile } */
/* { dg-options "-mcpu=xgene1" { target aarch64*-*-* } } */

typedef struct {
  int m1[10];
  double m2[10][8];
} blah;

void
foo (blah *info) {
  int i, d;

  for (d=0; d<10; d++) {
    info->m1[d] = 0;
    info->m2[d][0] = 1;
    for (i=1; i<8; i++)
      info->m2[d][i] = 2;
  }
}
