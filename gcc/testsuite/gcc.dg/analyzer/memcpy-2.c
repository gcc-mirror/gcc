/* { dg-additional-options "-Wno-stringop-overflow -Wno-analyzer-out-of-bounds" } */

void
main (int c, void *v)
{
  static char a[] = "";
  __builtin_memcpy (v, a, -1);
}
