/* { dg-options "-O0 -g" } */

typedef void (*isr_routine)(void);
isr_routine will_trig_exception;

 #if defined (__ARCHS__) || defined (__ARCEM__)
void __attribute__ ((interrupt("ilink")))
#else
void __attribute__ ((interrupt("ilink1")))
#endif
isr_template(void)
{
  will_trig_exception();
}

/* { dg-final { scan-assembler-times "\\\.cfi_offset 0" 1 } } */
