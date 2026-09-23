/* { dg-do compile } */
/* { dg-additional-options "-std=c99" } */

unsigned f (unsigned x)
{
  return (unsigned) (((unsigned long long) x * 0xAAAAAAAB) >> 32) >> 1;
}
