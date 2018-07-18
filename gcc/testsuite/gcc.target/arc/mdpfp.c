/* { dg-do compile } */
/* { dg-skip-if "FPX cannot execute on ARC HS" { archs } } */
/* { dg-options "-O2 -mdpfp" } */

double i;

int f (void)
{
        i *= 2.0;
}

/* { dg-final { scan-assembler "daddh" } } */
