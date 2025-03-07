/* { dg-additional-options "-fno-tree-ter" } */

int
foo(_Float16 f, int i)
{
  __builtin_memcpy(&i, &f, 2);
  return i;
}
