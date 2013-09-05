void __attribute__ ((interrupt("ilink1")))
handler1 (void)
{
}
/* { dg-final { scan-assembler-times "j.*\[ilink1\]" 1 } } */
