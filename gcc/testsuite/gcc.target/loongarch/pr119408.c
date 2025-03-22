/* { dg-do compile } */
/* { dg-options "-O2 -Wno-pedantic" } */

__float128 a;
__float128 b;
void
test (void)
{
  a = 1.11111111Q;
  b = 1.434345q;	
}

