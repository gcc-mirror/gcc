/* { dg-do compile } */
/* { dg-require-effective-target arm_eabi } */
/* { dg-options "-O2 -mabi=aapcs" } */


extern void __attribute__((weak)) wfunc(void);
void main(void)
{
  wfunc();  /* Must not tail-call.  */
}

/* { dg-final { scan-assembler-not "b\[\\t \]+wfunc" } } */
