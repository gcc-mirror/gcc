/* { dg-do compile } */ 
/* { dg-options "-O1 -fdump-tree-optimized --param sra-max-structure-size=32" } */

/* Test for SRA. */

typedef struct teststruct
{
  double d;
  char f1;
} teststruct;

teststruct *globf1;

extern void link_error (void);

void
copystruct1 (void)
{
  teststruct local;
  globf1->f1 = 0;
  local = *globf1;
  if (local.f1 != 0)
    link_error ();
}

/* There should be no reference to link_error. */
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
