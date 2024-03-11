/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zba_zbb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

void
foo(unsigned long a, unsigned long* ptr)
{
    ptr[0] = a & 0xffffffffUL;
    ptr[1] &= 0xffffffffUL;
}

void
foo2(unsigned long a, unsigned long* ptr)
{
    ptr[0] = a & 0xffff;
    ptr[1] &= 0xffff;
}

void
foo3(unsigned int a, unsigned int* ptr)
{
    ptr[0] = a & 0xffff;
    ptr[1] &= 0xffff;
}

/* { dg-final { scan-assembler-times {\mzext\.w\M} 1 } } */
/* { dg-final { scan-assembler-times {\mzext\.h\M} 2 } } */
/* { dg-final { scan-assembler-times {\mlwu} 1 } } */
/* { dg-final { scan-assembler-times {\mlhu} 2 } } */
/* { dg-final { scan-assembler-not "and\t" } } */
