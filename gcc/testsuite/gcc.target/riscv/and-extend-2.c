/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zba_zbb -mabi=ilp32" } */
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

/* { dg-final { scan-assembler-times {\mzext\.h\M} 2 } } */
/* { dg-final { scan-assembler-times {\mlhu} 2 } } */
/* { dg-final { scan-assembler-not "and\t" } } */
