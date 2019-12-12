/* { dg-options "-O2" } */
extern void will_trig_exception(void);

#if defined (__ARCHS__) || defined (__ARCEM__)
__attribute__ ((interrupt("ilink")))
#else
__attribute__ ((interrupt("ilink1")))
#endif
void isr_0 (void)
{
  will_trig_exception();
}

/* { dg-final { scan-assembler-times "j.*\[ilink1\]" 1 { target { arc6xx } } } } */
/* { dg-final { scan-assembler-times "rtie" 1 { target { ! { arc6xx } } } } } */
/* { dg-final { scan-assembler-times "blink" 2 } } */
/* { dg-final { scan-assembler-times "fp" 2 { target { ! { archs } } } } } */
/* { dg-final { scan-assembler-times "r30" 2 { target { archs || arcem } } } } */
/* { dg-final { scan-assembler-times "r24" 2 } } */
/* { dg-final { scan-assembler-times "r22" 2 } } */
/* { dg-final { scan-assembler-times "r20" 2 } } */
/* { dg-final { scan-assembler-times "r18" 2 } } */
/* { dg-final { scan-assembler-times "r16" 2 } } */
/* { dg-final { scan-assembler-times "r14" 2 } } */
/* { dg-final { scan-assembler-times "r12" 2 } } */
/* { dg-final { scan-assembler-times "r10" 2 } } */
/* { dg-final { scan-assembler-times "r8" 2 } } */
/* { dg-final { scan-assembler-times "r6" 2 } } */
/* { dg-final { scan-assembler-times "r4" 2 } } */
/* { dg-final { scan-assembler-times "r2\[,\n\]" 2 } } */
/* { dg-final { scan-assembler-times "lp_count" 2 } } */
/* { dg-final { scan-assembler-times "sr\\s+r\[0-9\]," 2 { target { ! { dpfp } } } } } */
/* { dg-final { scan-assembler-times "lr\\s+r\[0-9\]" 2 { target { ! { dpfp } } } } } */
/* { dg-final { scan-assembler-times "sr\\s+r\[0-9\]," 6 { target { dpfp } } } } */
/* { dg-final { scan-assembler-times "lr\\s+r\[0-9\]" 6 { target { dpfp } } } } */
/* { dg-final { scan-assembler-times "r58" 2 { target { accregs } } } } */
