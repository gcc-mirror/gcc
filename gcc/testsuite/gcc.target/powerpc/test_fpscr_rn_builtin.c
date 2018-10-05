/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-options "-O2 -std=c99" } */

#ifdef DEBUG
#include <stdio.h>
#endif

#define RN_MASK  0x3LL             /* RN field mask */

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

  /* __builtin_set_fpscr_rn() builtin can take a const or a variable
     value between 0 and 3 as the argument.
     __builtin_mtfsb0 and __builtin_mtfsb1 argument must be a constant 
     30 or 31.
  */

  /* Test reading the FPSCR register */
  __asm __volatile ("mffs %0" : "=f"(f14));
  conv_val.d = f14;

  if (conv_val.d != __builtin_mffs())
    {
#ifdef DEBUG
       printf("ERROR, __builtin_mffs() returned 0x%llx, not the expecected value 0x%llx\n",
	      __builtin_mffs(), conv_val.d);
#else
       abort();
#endif
    }		  

  /* Test float rounding mode builtin with const value argument.  */
  __builtin_set_fpscr_rn(3);
  conv_val.d = __builtin_mffs();
  ll_value = conv_val.ll & RN_MASK;

  if (ll_value != 3)
    {
#ifdef DEBUG
       printf("ERROR, __builtin_set_fpscr_rn(3) returned 0x%llx, not the expecected value 0x%x\n",
	      ll_value, 3);
#else
       abort();
#endif
    }		  

  val = 2;
  __builtin_set_fpscr_rn(val);
  conv_val.d = __builtin_mffs();
  ll_value = conv_val.ll & RN_MASK;

  if (ll_value != val)
    {
#ifdef DEBUG
       printf("ERROR, __builtin_set_fpscr_rn(val=%d) returned 0x%llx, not the expecected value 0x%x\n",
	      val, ll_value, val);
#else
       abort();
#endif
    }		  

  /* Reset to 0 for testing */
  val = 0;
  __builtin_set_fpscr_rn(val);

  __builtin_mtfsb1(31);
  conv_val.d = __builtin_mffs();
  ll_value = conv_val.ll & 0x1LL;

  if (ll_value != 1)
    {
#ifdef DEBUG
       printf("ERROR, __builtin_mtfsb1(31) did not set the bit to a 1.\n");
#else
       abort();
#endif
    }		  

  __builtin_mtfsb0(31);
  conv_val.d = __builtin_mffs();
  ll_value = conv_val.ll & 0x1LL;

  if (ll_value != 0)
    {
#ifdef DEBUG
       printf("ERROR, __builtin_mtfsb0(31) did not set the bit to a 0.\n");
#else
       abort();
#endif
    }		  

 __builtin_mtfsb1(30);
  conv_val.d = __builtin_mffs();
  ll_value = conv_val.ll & 0x2LL;

  if (ll_value != 2)
    {
#ifdef DEBUG
       printf("ERROR, __builtin_mtfsb1(31) did not set the bit to a 1.\n");
#else
       abort();
#endif
    }		  

  __builtin_mtfsb0(30);
  conv_val.d = __builtin_mffs();
  ll_value = conv_val.ll & 0x2LL;

  if (ll_value != 0)
    {
#ifdef DEBUG
       printf("ERROR, __builtin_mtfsb1(31) did not set the bit to a 0.\n");
#else
       abort();
#endif
    }		  

  __builtin_mtfsb1(0);
  conv_val.d = __builtin_mffs();
  ll_value = conv_val.ll & (0x1LL << (31-0));

  if (ll_value != (0x1LL << (31-0)))
    {
#ifdef DEBUG
       printf("ERROR, __builtin_mtfsb1(0) did not set the bit to a 1.\n");
#else
       abort();
#endif
    }		  

  __builtin_mtfsb0(0);
  conv_val.d = __builtin_mffs();
  ll_value = conv_val.ll & (0x1LL << (31-0));

  if (ll_value != 0)
    {
#ifdef DEBUG
       printf("ERROR, __builtin_mtfsb0(0) did not set the bit to a 0.\n");
#else
       abort();
#endif
    }		  


  /* Test builtin float rounding mode with variable as argument.  */
  val = 0;
  __builtin_set_fpscr_rn(val);
  conv_val.d = __builtin_mffs();
  ll_value = conv_val.ll & RN_MASK;

  if (ll_value != val)
    {
#ifdef DEBUG
       printf("ERROR, __builtin_set_fpscr_rn(val=%d) did not set rounding mode to %x.\n",
	      val, val);
#else
       abort();
#endif
    }		  

  val = 3;
  __builtin_set_fpscr_rn(val);
  conv_val.d = __builtin_mffs();
  ll_value = conv_val.ll & RN_MASK;

  if (ll_value != val)
    {
#ifdef DEBUG
       printf("ERROR, __builtin_set_fpscr_rn(val=%d) did not set rounding mode to %x.\n",
	      val, val);
#else
       abort();
#endif
    }		  
}
