/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre-details" } */

/* From PR14287.  */

short g, h;

void
foo (long a)
{
  short b = a & 3;
  long c = b;
  g = c;
  h = c;
}

/* { dg-final { scan-tree-dump "Replaced \\\(short int\\\) c_.*with b_" "fre" } } */
/* { dg-final { cleanup-tree-dump "fre" } } */
