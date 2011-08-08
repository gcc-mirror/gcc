/* Verify that SRA total scalarization will not be confused by padding.  */
/* Test skipped for targets with small (often default) MOVE_RATIO.  */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-release_ssa" } */

struct S
{
  int i;
  unsigned short f1;
  char f2;
  unsigned short f3, f4;
};


int foo (struct S *p)
{
  struct S l;

  l = *p;
  l.i++;
  *p = l;
}

/* { dg-final { scan-tree-dump-times "l;" 0 "release_ssa" { target { ! "avr*-*-*" } } } } */
/* { dg-final { cleanup-tree-dump "release_ssa" } } */
