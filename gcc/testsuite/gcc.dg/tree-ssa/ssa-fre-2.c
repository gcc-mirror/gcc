/* { dg-do compile } */
/* { dg-options "-O -fno-tree-forwprop -fdump-tree-fre1-details" } */

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

/* { dg-final { scan-tree-dump "Replaced \\\(short int\\\) c_.*with b_" "fre1" } } */
/* { dg-final { cleanup-tree-dump "fre1" } } */
