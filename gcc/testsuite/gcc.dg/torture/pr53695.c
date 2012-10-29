/* { dg-do compile } */
/* { dg-options "-ftracer" } */

void
foo (const void **p)
{
  void *labs[] = { &&l1, &&l2, &&l3 };
l1:
  goto *p++;
l2:
  goto *p;
l3:
  ;
}
