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
/* { dg-final { scan-assembler "baz:" } } */
/* { dg-final { scan-assembler "__acle_se_baz:" } } */
/* { dg-final { scan-assembler-not "\tcmse_nonsecure_caller" } } */
/* Look for an andsi of 1 with a register in function baz, ie.

;; Function baz<anything>
<any line without '('>
(insn <anything but '('> (set (reg<any register modifier>:SI <anything but ')'>)
     (and:SI (reg<any register modifier>:SI <anything but ')'>)
	     (const_int 1 <anything but ')'>)<anything but '('>
   <optional: (nil)<anything but '('>>
(insn
*/
/* { dg-final { scan-rtl-dump "\n;; Function baz\[^\n\]*\[^(\]+\[^;\]*\n\\(insn \[^(\]+ \\(set \\(reg\[^:\]*:SI \[^)\]+\\)\[^(\]*\\(and:SI \\(reg\[^:\]*:SI \[^)\]+\\)\[^(\]*\\((const_int 1|reg\[^:\]*:SI) \[^)\]+\\)\[^(\]+(\\(nil\\)\[^(\]+)?\\(insn" expand } } */

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
/* { dg-final { scan-assembler "qux:" } } */
/* { dg-final { scan-assembler "__acle_se_qux:" } } */
/* { dg-final { scan-assembler "bic" } } */
/* { dg-final { scan-assembler "push\t\{r4, r5, r6" } } */
/* { dg-final { scan-assembler "vstr\tFPCXTNS, \\\[sp, #-4\\\]!" { target arm_cmse_clear_ok } } } */
/* { dg-final { scan-assembler "vscclrm\t\{s0-s15, VPR\}" { target arm_cmse_clear_ok } } } */
/* { dg-final { scan-assembler "clrm\t\{r1, r2, r3, ip, APSR\}" { target arm_cmse_clear_ok } } } */
/* { dg-final { scan-assembler "vldr\tFPCXTNS, \\\[sp\\\], #4" { target arm_cmse_clear_ok } } } */
/* { dg-final { scan-assembler "msr\tAPSR_nzcvq" { target { ! arm_cmse_clear_ok } } } } */
/* { dg-final { scan-assembler "push\t\{r4, r5, r6, r7, r8, r9, r10, fp\}" { target arm_cmse_clear_ok } } } */
/* Check the right registers are cleared and none appears twice.  */
/* { dg-final { scan-assembler "clrm\t\{(r0, )?(r1, )?(r2, )?(r3, )?(r4, )?(r5, )?(r6, )?(r7, )?(r8, )?(r9, )?(r10, )?(fp, )?(ip, )?APSR\}" { target arm_cmse_clear_ok } } } */
/* Check that the right number of registers is cleared and thus only one
   register is missing.  */
/* { dg-final { scan-assembler "clrm\t\{((r\[0-9\]|r10|fp|ip), ){12}APSR\}" { target arm_cmse_clear_ok } } } */
/* Check that no cleared register is used for blxns.  */
/* { dg-final { scan-assembler-not "clrm\t\{\[^\}\]\+(r\[0-9\]|r10|fp|ip),\[^\}\]\+\}(?!.*clrm.*blxns).*blxns\t\\1" { target arm_cmse_clear_ok } } } */
/* { dg-final { scan-assembler "pop\t\{r4, r5, r6, r7, r8, r9, r10, fp\}" { target arm_cmse_clear_ok } } } */

int call_callback (void)
{
  if (cmse_is_nsfptr (fp))
      return fp ();
  else
    return default_callback ();
}
/* { dg-final { scan-assembler-times "bl\\s+__gnu_cmse_nonsecure_call" 1 { target { ! arm_cmse_clear_ok } } } } */
/* { dg-final { scan-assembler "blxns" { target arm_cmse_clear_ok } } } */
