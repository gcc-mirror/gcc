void __attribute__ ((interrupt("ilink2")))
handler1 (void)
{
}
/* { dg-final { scan-assembler-times "j.*\[ilink2\]" 1 } } */
