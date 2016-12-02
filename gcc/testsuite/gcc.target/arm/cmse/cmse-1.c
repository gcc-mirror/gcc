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
