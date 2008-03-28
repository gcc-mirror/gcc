/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned char x;
int foo(void)
{
  unsigned long long i = x;
  i = i + 0x80000000;
  if (i > 0xffffffff)
    return x;
  return 0;
}
