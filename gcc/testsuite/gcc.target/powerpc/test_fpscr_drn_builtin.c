/* { dg-do run { target { powerpc*-*-* &&  lp64 } } } */
/* { dg-require-effective-target hard_dfp } */
/* { dg-options "-O2 -std=c99" } */

#ifdef DEBUG
#include <stdio.h>
#endif

#define DRN_MASK 0x700000000LL     /* DRN field mask */

void abort (void);

int main ()
{
  int i;
  int val, bit;
  double fpscr_val;
  union blah {
    double d;
    unsigned long long ll;
  } conv_val;
  
  unsigned long long ll_value;
  register double  f14;

  /* __builtin_set_fpscr_drn() builtin can take a const or a variable
     value between 0 and 7 as the argument.
  */

  /* Test builtin decimal float rounding mode with const argument.  */
  __builtin_set_fpscr_drn(7);
  conv_val.d = __builtin_mffs();
  ll_value = conv_val.ll & DRN_MASK;

  if (ll_value != 0x700000000)
    {
#ifdef DEBUG
       printf("ERROR, __builtin_set_fpscr_drn(7) did not set rounding mode to 7.\n");
#else
       abort();
#endif
    }		  

  __builtin_set_fpscr_drn(2);
  conv_val.d = __builtin_mffs();
  ll_value = conv_val.ll & DRN_MASK;

  if (ll_value != 0x200000000)
    {
#ifdef DEBUG
       printf("ERROR, __builtin_set_fpscr_drn(2) did not set rounding mode to 2.\n");
#else
       abort();
#endif
    }		  

  __builtin_set_fpscr_drn(5);
  conv_val.d = __builtin_mffs();
  ll_value = conv_val.ll & DRN_MASK;

  if (ll_value != 0x500000000)
    {
#ifdef DEBUG
       printf("ERROR, __builtin_set_fpscr_drn(5) did not set rounding mode to 5.\n");
#else
       abort();
#endif
    }		  

  /* Test builtin decimal float rounding mode with variable as argument.  */
  val = 7;
  __builtin_set_fpscr_drn(val);
  conv_val.d = __builtin_mffs();
  ll_value = conv_val.ll & DRN_MASK;

  if (ll_value != ((unsigned long long)val << 32))
    {
#ifdef DEBUG
       printf("ERROR, __builtin_set_fpscr_drn(val=%d) did not set rounding mode to %d.\n",
	      val, val);
#else
       abort();
#endif
    }		  

  val = 0;
  __builtin_set_fpscr_drn(val);
  conv_val.d = __builtin_mffs();
  ll_value = conv_val.ll & DRN_MASK;

  if (ll_value != ((unsigned long long)val << 32))
    {
#ifdef DEBUG
       printf("ERROR, __builtin_set_fpscr_drn(val=%d) did not set rounding mode to %d.\n",
	      val, val);
#else
       abort();
#endif
    }		  

  val = 2;
  __builtin_set_fpscr_drn(val);
  conv_val.d = __builtin_mffs();
  ll_value = conv_val.ll & DRN_MASK;

  if (ll_value != ((unsigned long long)val << 32))
    {
#ifdef DEBUG
       printf("ERROR, __builtin_set_fpscr_drn(val=%d) did not set rounding mode to %d.\n",
	      val, val);
#else
       abort();
#endif
    }	  
}
