/* { dg-do compile } */
/* { dg-options "-fstrict-overflow -O2 -Wstrict-overflow" } */

/* Don't warn about an overflow in a copied loop header.  We used to
   get a warning in value numbering.  This is PR 33565.  */

void f (int m, int n)
{
  int j;
 
  for (j = m; j	< m + 10 && j <	n; j ++)
    do_something (j);
}
