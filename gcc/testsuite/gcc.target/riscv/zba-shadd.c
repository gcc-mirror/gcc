/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zba -mabi=lp64" } */

unsigned long foo(unsigned int a, unsigned long b)
{
        a = a << 1;
        unsigned long c = (unsigned long) a;
        unsigned long d = b + (c<<2);
        return d;
}

/* { dg-final { scan-assembler "sh2add.uw" } } */
/* { dg-final { scan-assembler-not "zext" } } */