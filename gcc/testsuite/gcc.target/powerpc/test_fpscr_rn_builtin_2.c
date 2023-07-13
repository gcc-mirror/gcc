/* { dg-do run { target powerpc_fprs } } */
/* { dg-options "-O2" } */

/* The __builtin_set_fpscr_rn built-in was originally defined to return
   void.  The built-in now returns a double containing the various FPSCR bits.
   This test verifies the new return value.  */

#ifdef DEBUG
#include <stdio.h>
#endif

#define RN_MASK 0x3LL             /* RN field mask */
#define FIELD_MASK 0x00000007000000FFULL

union blah {
  double d;
  unsigned long long ll;
} conv_val;

void abort (void);

double __attribute__ ((noipa)) wrap_set_fpscr_rn (int val)
{
  return __builtin_set_fpscr_rn (val);
}

double __attribute__ ((noipa)) wrap_const_fpscr_rn (int val)
{
  switch (val)
    {
    case 0: return __builtin_set_fpscr_rn (0x0);
    case 1: return __builtin_set_fpscr_rn (0x1);
    case 2: return __builtin_set_fpscr_rn (0x2);
    case 3: return __builtin_set_fpscr_rn (0x3);
    }
}

void check_builtin_set_fpscr_rn (unsigned long long initial_fpscr,
				 int new_RN, double result)
{
  register double  f14;
  unsigned long long masked_fpscr = initial_fpscr & FIELD_MASK;
  
  conv_val.d = result;

  /* Check the result.  */
  if (conv_val.ll != masked_fpscr)
    {
#ifdef DEBUG
       printf("ERROR, __builtin_set_fpscr_rn(%d) did not return expected value %llx.\n",
	      new_RN, masked_fpscr);
       printf("fpscr_val_initial = 0x%llx\n", initial_fpscr);       
       printf("result = 0x%llx\n", conv_val.ll);
#else
       abort();
#endif
    }

  /* Check to see if the RN field was updated.  */
  __asm __volatile ("mffs %0" : "=f"(f14));
  conv_val.d = f14;

  if ((conv_val.ll & RN_MASK) != new_RN)
#ifdef DEBUG
    {
      printf("ERROR,  __builtin_set_fpscr_rn(%d) did not update RN to %llx.\n",
	     new_RN, new_RN);
      printf("  conv_val.ll = 0x%llx\n", conv_val.ll);
    }
#else
    abort();
#endif
}

int
main ()
{
  int i;
  int val, bit;
  double fpscr_val;
  unsigned long long fpscr_val_initial;
  
  unsigned long long ll_value;
  union blah src_double;
  register double  f14;

  /* If __SET_FPSCR_RN_RETURNS_FPSCR__ is defined, the __builtin_set_fpscr_rn()
     builtin returns the FPSCR fields.*/
  
  /* __builtin_set_fpscr_rn() builtin can take a constant or a variable
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
       printf("ERROR, __builtin_mffs() returned 0x%llx, not the expected value 0x%llx\n",
	      __builtin_mffs(), conv_val.d);
#else
       abort();
#endif
    }		  

  /* Test return value from __builtin_set_fpscr_rn. The FPSCR fields (DRN, VE,
     OE, UE, ZE, XE, NI, RN) are returned and  the RN field of FPSCR is updated
     with the specified argument for the built-in.  */

  /* Check immediate argument cases */
  __asm __volatile ("mffs %0" : "=f"(f14));
  conv_val.d = f14;
  fpscr_val_initial = conv_val.ll;

  val = 0x0;
  fpscr_val = wrap_const_fpscr_rn (val);
  check_builtin_set_fpscr_rn (fpscr_val_initial, val, fpscr_val);

  __asm __volatile ("mffs %0" : "=f"(f14));
  conv_val.d = f14;
  fpscr_val_initial = conv_val.ll;

  val = 0x3;
  fpscr_val = wrap_const_fpscr_rn (val);
  check_builtin_set_fpscr_rn (fpscr_val_initial, val, fpscr_val);
  
  /* Check int argument cases */
  __asm __volatile ("mffs %0" : "=f"(f14));
  conv_val.d = f14;
  fpscr_val_initial = conv_val.ll;

  val = 0x1;
  fpscr_val = wrap_set_fpscr_rn (val);
  check_builtin_set_fpscr_rn (fpscr_val_initial, val, fpscr_val);

  __asm __volatile ("mffs %0" : "=f"(f14));
  conv_val.d = f14;
  fpscr_val_initial = conv_val.ll;

  val = 0x2;
  fpscr_val = wrap_set_fpscr_rn (val);
  check_builtin_set_fpscr_rn (fpscr_val_initial, val, fpscr_val);
  return 0;
}		  
