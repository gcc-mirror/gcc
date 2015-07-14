/* { dg-do assemble } */
/* { dg-options "-O" } */

void test_flushd (unsigned char* p1, unsigned char* p2)
{
  __builtin_flushd (p1);
  __builtin_flushd (p2);
  __builtin_flushd (p2 + 1);
  __builtin_flushd (p2 + 2);
  __builtin_flushd (p2 + 2047);
  __builtin_flushd (p2 + 2048);
}

void test_flushda (unsigned char* p1, unsigned char* p2)
{
  __builtin_flushda (p1);
  __builtin_flushda (p2);
  __builtin_flushda (p2 + 1);
  __builtin_flushda (p2 + 2);
  __builtin_flushda (p2 + 2047);
  __builtin_flushda (p2 + 2048);
}
