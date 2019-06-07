/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-g -O -mx32 -maddress-mode=long" } */

extern void get_BID128 (int *);
void 
__bid128_div (void)
{
  int res;
  get_BID128 (&res);
}
