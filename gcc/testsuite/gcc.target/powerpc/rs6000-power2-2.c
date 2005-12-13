/* { dg-do compile { target { ilp32 } } } */
/* { dg-options "-O3 -mcpu=power2 -fno-schedule-insns -w -mhard-float" } */
/* { dg-final { scan-assembler-not "lfd" } } */
/* { dg-final { scan-assembler-not "sfd" } } */
/* { dg-final { scan-assembler "lfq" } } */
/* { dg-final { scan-assembler "stfq" } } */

register volatile double t1 __asm__("fr0");
register volatile double t2 __asm__("fr1");
register volatile double t3 __asm__("fr2"), t4 __asm__("fr3");
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

