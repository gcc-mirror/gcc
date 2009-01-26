/* { dg-do compile } */
/* { dg-options "-O2" } */

/* This variable needed only to exercise FRE instead of CCP.  */
unsigned char g;

extern void abort();

void f (long long int p)
{
  g = 255;
  if (p >= (-9223372036854775807LL - 1) - (signed char) g)
    p = 1;

  if (p)
    abort ();
}


