/* Verify that we can narrow the storage associated with label diffs.  */

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
