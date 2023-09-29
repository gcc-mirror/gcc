/* { dg-additional-options "-Wno-stringop-overflow -Wno-analyzer-out-of-bounds" } */

int
test (int c, void *v)
{
  static char a[] = "";
  __builtin_memcpy (v, a, -1);
  return 0;
}
