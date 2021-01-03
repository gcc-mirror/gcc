/* { dg-do assemble } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -msecure-plt -fPIC" } */
/* { dg-skip-if "" { powerpc*-*-darwin* powerpc-ibm-aix* } } */

#define FE_TONEAREST 0
#define FE_UPWARD 1
#define FE_DOWNWARD 2
#define FE_TOWARDZERO 3

extern int fesetround(int);

void
set_fpu_rounding_mode (int mode)
{
  int rnd_mode;

  switch (mode)
    {
      case 2:
       rnd_mode = FE_TONEAREST;
       break;

      case 4:
        rnd_mode = FE_UPWARD;
        break;

      case 1:
        rnd_mode = FE_DOWNWARD;
        break;

      case 3:
        rnd_mode = FE_TOWARDZERO; 
        break;

      default:
        return;
    }

  fesetround (rnd_mode);
}
