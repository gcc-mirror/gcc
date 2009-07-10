/* PR middle-end/38934 */
/* { dg-do compile } */
/* { dg-options "-O2 -std=gnu99 -w" } */

/* This variable needed only to work around earlier optimizations than VRP.  */
unsigned char g;

extern void abort ();

void
f (long long int p)
{
  g = 255;
  if (p >= -9223372036854775808LL - (signed char) g)
    p = 1;

  if (p)
    abort ();
}
