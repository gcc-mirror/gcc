/* Test whether variables with relocations aren't put into
   mergeable sections even with -fmerge-all-constants.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fmerge-all-constants" } */
/* { dg-final { scan-assembler-not ".rodata.cst" } } */

int foo (int a)
{
  static void * const ar[] = { &&l2 };
  void *p = ar[a];
  goto *p;
l2:
  return 2;
}
