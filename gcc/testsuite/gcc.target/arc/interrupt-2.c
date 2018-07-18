/* { dg-skip-if "ilink2 is not an ARCv2 register" { archs || arcem } } */
void __attribute__ ((interrupt("ilink2")))
handler1 (void)
{
}
/* { dg-final { scan-assembler-times "j.*\[ilink2\]" 1 } } */
