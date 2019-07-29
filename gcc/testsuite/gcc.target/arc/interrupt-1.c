#if defined (__ARCHS__) || defined (__ARCEM__)
void __attribute__ ((interrupt("ilink")))
#else
void __attribute__ ((interrupt("ilink1")))
#endif
handler1 (void)
{
}
/* { dg-final { scan-assembler-times "j.*\[ilink1\]" 1 { target { arc6xx } } } } */
/* { dg-final { scan-assembler-times "rtie" 1 { target { ! { arc6xx } } } } } */
