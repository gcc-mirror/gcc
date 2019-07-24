extern int a;

#if defined (__ARCHS__) || defined (__ARCEM__)
__attribute__ ((interrupt("ilink")))
#else
__attribute__ ((interrupt("ilink2")))
#endif
void isr_1 (void)
{
  a++;
}

/* { dg-final { scan-assembler-times "j.*\[ilink2\]" 1 { target { arc6xx } } } } */
/* { dg-final { scan-assembler-times "rtie" 1 { target { ! { arc6xx } } } } } */
/* { dg-final { scan-assembler-times "push_s\\s+r\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "pop_s\\s+r\[0-9\]" 1 } } */
