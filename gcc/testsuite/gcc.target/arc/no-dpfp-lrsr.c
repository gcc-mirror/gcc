/* { dg-do compile } */
/* { dg-options "-O2 -mdpfp -mno-dpfp-lrsr" } */

double i;

int f (void)
{
        i *= 2.0;
}

/* { dg-final { scan-assembler-not "\tlr" } } */
