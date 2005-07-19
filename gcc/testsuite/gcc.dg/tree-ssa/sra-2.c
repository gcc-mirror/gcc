/* { dg-do compile } */ 
/* { dg-options "-O1 -fdump-tree-optimized --param sra-max-structure-size=32" } */

/* Test for SRA. */

typedef struct teststruct
{
  double d;
  char f1;
} teststruct;


void
copystruct11 (teststruct *param)
{
  static teststruct local;
  param->f1 = 0;
  local = *param;
  if (local.f1 != 0)
    link_error ();
}


/* There should be no reference to link_error. */
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
