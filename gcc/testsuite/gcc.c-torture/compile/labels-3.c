/* Verify that we can narrow the storage associated with label diffs.  */
/* { dg-require-effective-target indirect_jumps } */
/* { dg-require-effective-target label_values } */

int foo (int a)
{
  static const short ar[] = { &&l1 - &&l1, &&l2 - &&l1 };
  void *p = &&l1 + ar[a];
  goto *p;
 l1:
  return 1;
 l2:
  return 2;
}
