/* Verify strlen results of vector assignments.
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

#include "strlenopt.h"

#define V(N) __attribute__ ((vector_size (N)))

typedef V (1) char VC1;
typedef V (2) char VC2;
typedef V (4) char VC4;
typedef V (8) char VC8;
typedef V (16) char VC16;

extern char a[];

#define A(expr) ((expr) ? (void)0 : abort ())

void test_fold (int i)
{
  *(VC4*)a = (VC4){ };
  A (strlen (a) == 0);
  A (!a[1] && !a[2] && !a[3]);

  *(VC4*)a = (VC4){ 0, 1 };
  A (strlen (a) == 0);
  A (a[1] == 1 && !a[2] && !a[3]);

  *(VC4*)a = (VC4){ 1 };
  A (strlen (a) == 1);
  A (!a[1] && !a[2] && !a[3]);

  *(VC4*)a = (VC4){ 1, 0, 3 };
  A (strlen (a) == 1);
  A (!a[1] && a[2] == 3 && !a[3]);

  *(VC4*)a = (VC4){ 1, 2 };
  A (strlen (a) == 2);
  A (!a[2] && !a[3]);

  *(VC4*)a = (VC4){ 1, 2, 0, 4 };
  A (strlen (a) == 2);
  A (!a[2] && a[3] == 4);

  *(VC4*)a = (VC4){ 1, 2, 3 };
  A (strlen (a) == 3);
  A (!a[3]);

  *(VC8*)a = (VC8){ 1, 2, 3, 0, 5 };
  A (strlen (a) == 3);

  *(VC8*)a = (VC8){ 1, 2, 3, 0, 5, 6 };
  A (strlen (a) == 3);

  *(VC8*)a = (VC8){ 1, 2, 3, 0, 5, 6, 7, 8 };
  A (strlen (a) == 3);
  A (strlen (a + 1) == 2);
  A (strlen (a + 2) == 1);
  A (strlen (a + 3) == 0);

  A (a[4] == 5 && a[5] == 6 && a[6] == 7 && a[7] == 8);
}

/* { dg-final { scan-tree-dump-not "abort \\(" "optimized" } }
   { dg-final { scan-tree-dump-not "strlen \\(" "optimized" } } */
