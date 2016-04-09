/* { dg-do compile } */
/* { dg-options "-O1 -fdump-rtl-expand-details" } */
/* { dg-skip-if "PR64886" { hppa*-*-hpux* } { "*" } { "" } } */

#define N 256
int a1[N], a2[N], a3[N], a4[N];

void foo ()
{
  int i;
  for (i=0; i<N; i++) {
    int c;
    c = a3[i] + (a1[i] * a2[i]);
    a4[i] = c + 1;
    a1[i] = a2[i] - 1;
  }
}

/* { dg-final { scan-rtl-dump-times "Swap operands" 1 "expand" } } */


