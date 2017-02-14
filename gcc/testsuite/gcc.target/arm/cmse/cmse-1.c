/* { dg-do compile } */
/* { dg-options "-Os -mcmse -fdump-rtl-expand" }  */

#include <arm_cmse.h>

extern int a;
extern int bar (void);

int foo (char * p)
{
  cmse_address_info_t cait;

  cait = cmse_TT (&a);
  if (cait.flags.mpu_region)
    a++;

  cait = cmse_TT_fptr (&bar);
  if (cait.flags.mpu_region)
    a+= bar ();

  cait = cmse_TTA (&a);
  if (cait.flags.mpu_region)
    a++;

  cait = cmse_TTA_fptr (&bar);
  if (cait.flags.mpu_region)
    a+= bar ();

  cait = cmse_TTT (&a);
  if (cait.flags.mpu_region)
    a++;

  cait = cmse_TTT_fptr (&bar);
  if (cait.flags.mpu_region)
    a+= bar ();

  cait = cmse_TTAT (&a);
  if (cait.flags.mpu_region)
    a++;

  cait = cmse_TTAT_fptr (&bar);
  if (cait.flags.mpu_region)
    a+= bar ();

  p = (char *) cmse_check_address_range ((void *) p, sizeof (char), 0);
  p = (char *) cmse_check_address_range ((void *) p, sizeof (char),
					 CMSE_MPU_UNPRIV);
  p = (char *) cmse_check_address_range ((void *) p, sizeof (char),
					 CMSE_MPU_READWRITE);
  p = (char *) cmse_check_address_range ((void *) p, sizeof (char),
					 CMSE_MPU_UNPRIV | CMSE_MPU_READ);
  p = (char *) cmse_check_address_range ((void *) p, sizeof (char),
					 CMSE_AU_NONSECURE
					 | CMSE_MPU_NONSECURE);
  p = (char *) cmse_check_address_range ((void *) p, sizeof (char),
					 CMSE_NONSECURE | CMSE_MPU_UNPRIV);

  p = (char *) cmse_check_pointed_object (p, CMSE_NONSECURE | CMSE_MPU_UNPRIV);

  return a;
}
/* { dg-final { scan-assembler-times "\ttt " 2 } } */
/* { dg-final { scan-assembler-times "ttt " 2 } } */
/* { dg-final { scan-assembler-times "tta " 2 } } */
/* { dg-final { scan-assembler-times "ttat " 2 } } */
/* { dg-final { scan-assembler-times "bl.cmse_check_address_range" 7 } } */
/* { dg-final { scan-assembler-not "cmse_check_pointed_object" } } */

int __attribute__ ((cmse_nonsecure_entry))
baz (void)
{
  return cmse_nonsecure_caller ();
}

typedef int __attribute__ ((cmse_nonsecure_call)) (int_nsfunc_t) (void);

int default_callback (void)
{
  return 0;
}

int_nsfunc_t * fp = (int_nsfunc_t *) default_callback;

void __attribute__ ((cmse_nonsecure_entry))
qux (int_nsfunc_t * callback)
{
  fp = cmse_nsfptr_create (callback);
}

int call_callback (void)
{
  if (cmse_is_nsfptr (fp))
      return fp ();
  else
    return default_callback ();
}
/* { dg-final { scan-assembler "baz:" } } */
/* { dg-final { scan-assembler "__acle_se_baz:" } } */
/* { dg-final { scan-assembler "qux:" } } */
/* { dg-final { scan-assembler "__acle_se_qux:" } } */
/* { dg-final { scan-assembler-not "\tcmse_nonsecure_caller" } } */
/* { dg-final { scan-rtl-dump "and.*reg.*const_int 1" expand } } */
/* { dg-final { scan-assembler "bic" } } */
/* { dg-final { scan-assembler "push\t\{r4, r5, r6" } } */
/* { dg-final { scan-assembler "msr\tAPSR_nzcvq" } } */
/* { dg-final { scan-assembler-times "bl\\s+__gnu_cmse_nonsecure_call" 1 } } */
