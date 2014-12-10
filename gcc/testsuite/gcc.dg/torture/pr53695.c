/* { dg-do compile } */
/* { dg-options "-ftracer" } */
/* { dg-require-effective-target indirect_jumps } */
/* { dg-require-effective-target label_values } */

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
