/* PR tree-optimization/46309 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-reassoc-details" } */
/* The transformation depends on BRANCH_COST being greater than 1
   (see the notes in the PR), so try to force that.  */
/* { dg-additional-options "-mtune=octeon2" { target mips*-*-* } } */
/* { dg-additional-options "-mbranch-cost=2" { target avr*-*-* s390*-*-* } } */

int
f1 (int a)
{
  int v1 = (a == 3);
  int v2 = (a == 1);
  int v3 = (a == 4);
  int v4 = (a == 2);
  return v1 || v2 || v3 || v4;
}

int
f2 (int a)
{
  int v1 = (a == 1);
  int v2 = (a == 2);
  int v3 = (a == 3);
  int v4 = (a == 4);
  return v1 || v2 || v3 || v4;
}

int
f3 (int a)
{
  int v1 = (a == 3);
  int v2 = (a == 1);
  return v1 || v2;
}

int
f4 (int a)
{
  int v1 = (a == 1);
  int v2 = (a == 2);
  return v1 || v2;
}

int
f5 (unsigned int a)
{
  int v1 = (a <= 31);
  int v2 = (a >= 64 && a <= 95);
  return v1 || v2;
}

int
f6 (unsigned int a)
{
  int v1 = (a <= 31);
  int v2 = (a >= 64 && a <= 95);
  int v3 = (a >= 128 && a <= 159);
  int v4 = (a >= 192 && a <= 223);
  return v1 || v2 || v3 || v4;
}

/* { dg-final { scan-tree-dump-times "Optimizing range tests a_\[0-9\]*.D. -.1, 1. and -.2, 2. and -.3, 3. and -.4, 4.\[\n\r\]* into" 2 "reassoc1" } } */
/* { dg-final { scan-tree-dump-times "Optimizing range tests a_\[0-9\]*.D. -.1, 1. and -.3, 3.\[\n\r\]* into" 1 "reassoc1" } } */
/* { dg-final { scan-tree-dump-times "Optimizing range tests a_\[0-9\]*.D. -.1, 1. and -.2, 2.\[\n\r\]* into" 1 "reassoc1" } } */
/* { dg-final { scan-tree-dump-times "Optimizing range tests a_\[0-9\]*.D. -.0, 31. and -.64, 95.\[\n\r\]* into" 2 "reassoc1" } } */
/* { dg-final { scan-tree-dump-times "Optimizing range tests a_\[0-9\]*.D. -.128, 159. and -.192, 223.\[\n\r\]* into" 1 "reassoc1" } } */
/* { dg-final { scan-tree-dump-times "Optimizing range tests \[^\r\n\]*_\[0-9\]* -.0, 31. and -.128, 159.\[\n\r\]* into" 1 "reassoc2" } } */
/* { dg-final { cleanup-tree-dump "reassoc1" } } */
/* { dg-final { cleanup-tree-dump "reassoc2" } } */
