/* { dg-do compile } */
/* { dg-options "-O2 -mspfp" } */

float i;

int f (void)
{
        i *= 2.0;
}

/* { dg-final { scan-assembler "fadd" } } */
