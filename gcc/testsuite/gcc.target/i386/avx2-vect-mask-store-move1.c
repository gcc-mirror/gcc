/* { dg-options "-O3 -mavx2 -fdump-tree-vect-details" } */
/* { dg-require-effective-target avx2 } */
/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */

#include "avx2-check.h"
#define N 32
int *p1, *p2, *p3;
int c[N];
int p1ref[N], p2ref[N];

__attribute__((noinline, noclone)) void foo (int n)
{
  int i;
  for (i=0; i<n; i++)
    if (c[i])
      {
	p1[i] += 1;
	p2[i] = p3[i] +2;
      }
}

void init ()
{
  p1ref[0]=1; p2ref[0]=2;
  p1ref[1]=3; p2ref[1]=5;
  p1ref[2]=5; p2ref[2]=8;
  p1ref[3]=7; p2ref[3]=11;
  p1ref[4]=9; p2ref[4]=14;
  p1ref[5]=11; p2ref[5]=17;
  p1ref[6]=13; p2ref[6]=20;
  p1ref[7]=15; p2ref[7]=23;
  p1ref[8]=16; p2ref[8]=8;
  p1ref[9]=18; p2ref[9]=9;
  p1ref[10]=20; p2ref[10]=10;
  p1ref[11]=22; p2ref[11]=11;
  p1ref[12]=24; p2ref[12]=12;
  p1ref[13]=26; p2ref[13]=13;
  p1ref[14]=28; p2ref[14]=14;
  p1ref[15]=30; p2ref[15]=15;
  p1ref[16]=33; p2ref[16]=50;
  p1ref[17]=35; p2ref[17]=53;
  p1ref[18]=37; p2ref[18]=56;
  p1ref[19]=39; p2ref[19]=59;
  p1ref[20]=41; p2ref[20]=62;
  p1ref[21]=43; p2ref[21]=65;
  p1ref[22]=45; p2ref[22]=68;
  p1ref[23]=47; p2ref[23]=71;
  p1ref[24]=48; p2ref[24]=24;
  p1ref[25]=50; p2ref[25]=25;
  p1ref[26]=52; p2ref[26]=26;
  p1ref[27]=54; p2ref[27]=27;
  p1ref[28]=56; p2ref[28]=28;
  p1ref[29]=58; p2ref[29]=29;
  p1ref[30]=60; p2ref[30]=30;
  p1ref[31]=62; p2ref[31]=31;
}

static void
avx2_test (void)
{
  int * P = malloc (N * 3 * sizeof (int));
  int i;

  p1 = &P[0];
  p2 = &P[N];
  p3 = &P[2 * N];
  for (i=0; i<N; i++) {
    p1[i] = i + i;
    p3[i] = i * 3;
    p2[i] = i;
    c[i] = (i >> 3) & 1? 0: 1;
  }
  init ();
  foo (N);
  for (i=0; i<N;i++)
    if (p1[i] != p1ref[i] || p2[i] != p2ref[i])
      abort ();
}

/* { dg-final { scan-tree-dump-times "Move stmt to created bb" 10 "vect" } } */
