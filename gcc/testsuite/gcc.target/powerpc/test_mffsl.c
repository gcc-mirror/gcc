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
  } conv_val;

  /* Test reading the FPSCR register.  */
  __asm __volatile ("mffs %0" : "=f"(f14));
  conv_val.d = f14;

  if (conv_val.d != __builtin_mffsl())
    {
#ifdef DEBUG
      printf("ERROR, __builtin_mffsl() returned 0x%llx, not the expecected value 0x%llx\n",
	     __builtin_mffsl(), conv_val.d);
#else
      abort();
#endif
    }		  
}
