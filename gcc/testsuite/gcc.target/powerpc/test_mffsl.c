/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-options "-O2 -std=c99" } */

#ifdef DEBUG
#include <stdio.h>
#endif

void abort (void);

int main ()
{

  register double  f14;
  union blah {
    double d;
    unsigned long long ll;
  } mffs_val, mffsl_val;

  /* Test reading the FPSCR register.  */
  __asm __volatile ("mffs %0" : "=f"(f14));
  mffs_val.d = f14;
  /* Select the same bits as mffsl.  */
  mffs_val.ll &= 0x70007f0ffLL;

  mffsl_val.d = __builtin_mffsl ();

  if (mffs_val.ll != mffsl_val.ll)
    {
#ifdef DEBUG
      printf("ERROR, __builtin_mffsl() returned 0x%llx, not the expecected value 0x%llx\n",
	     mffsl_val.ll, mffs_val.ll);
#else
      abort();
#endif
    }		  
}
