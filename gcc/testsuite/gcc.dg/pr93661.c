/* { dg-do compile } */
/* { dg-options "-O2 -w" } */

int f ()
{
  unsigned x = 0xffffffff;
  __builtin_memset (1+(char *) &x, 0, -1);
  return (x != 0xf0000000);
}
