/* { dg-do compile } */
/* { dg-options "-O2" } */

int f ()
{
  unsigned x = 0xffffffff;
  __builtin_memset (1+(char *) &x, 0, -1); /* { dg-warning "maximum object size" } */
  return (x != 0xf0000000);
}
