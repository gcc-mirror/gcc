/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_dsp } */

int
foo (int a, short b, short c)
{
     int bc = b * c;
        return a + (short)bc;
}

/* { dg-final { scan-assembler "\tmul\t" } } */
