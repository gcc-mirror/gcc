/* { dg-do compile } */
/* { dg-options "-O2 -mmpx -fcheck-pointer-bounds" } */

char *
foo (char *p, char *q)
{
  return (char *) (p - q);	/* { dg-bogus "pointer bounds were lost due to unexpected expression" } */
}
