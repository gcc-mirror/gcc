/* { dg-additional-options "-Wno-stringop-overflow" } */

void
main (int c, void *v)
{
  static char a[] = "";
  __builtin_memcpy (v, a, -1);
}
