/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -fplt -mtls-dialect=gnu" } */

extern __thread int __bid_IDEC_glbflags;
extern long __bid64qq_div_bid_y_0_1;
extern void get_BID64(int *);
void
__bid64qq_div(void)
{
  if (__bid64qq_div_bid_y_0_1)
    __bid_IDEC_glbflags |= 1;
  get_BID64(&__bid_IDEC_glbflags);
}
