/* { dg-do assemble { target powerpc-*-* rs6000-*-* }  } */
/* { dg-options "-O -mpower2 -fno-schedule-insns" } */
/* { dg-final { scan-assembler-not "lfd" } } */
/* { dg-final { scan-assembler-not "sfd" } } */
/* { dg-final { scan-assembler "lfq" } } */
/* { dg-final { scan-assembler "sfq" } } */

register double t1 __asm__("f0");
register double t2 __asm__("f1");
register double t3 __asm__("f2"), t4 __asm__("f3");
void t(double *a, double *b)
{
        t1 = a[-1];
        t2 = a[0];
        t3 = a[1];
        t4 = a[2];
        b[-1] = t1;
        b[0] = t2;
        b[1] = t3;
        b[2] = t4;
}

