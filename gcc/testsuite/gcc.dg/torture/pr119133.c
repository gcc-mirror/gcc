/* { dg-additional-options "-fno-tree-ter" } */
/* { dg-require-effective-target float16 } */
/* { dg-add-options float16 } */

int
foo(_Float16 f, int i)
{
  __builtin_memcpy(&i, &f, 2);
  return i;
}
