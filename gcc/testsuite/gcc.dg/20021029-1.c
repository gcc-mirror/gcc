/* Test whether difference of local labels doesn't force
   variables into writable sections.  */
/* { dg-do compile { target fpic } } */
/* { dg-options "-O2 -fpic" } */
/* { dg-final { scan-assembler-not ".data.rel.ro.local" } } */
/* { dg-require-effective-target label_values } */
/* { dg-require-effective-target indirect_jumps } */

int foo (int a)
{
  static const int ar[] = { &&l1 - &&l1, &&l2 - &&l1 };
  void *p = &&l1 + ar[a];
  goto *p;
  l1:
    return 1;
  l2:
    return 2;
}
