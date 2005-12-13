/* { dg-do compile { target { ilp32 } } } */
/* { dg-options "-O3 -mcpu=power2 -fno-schedule-insns -w -mhard-float" } */
/* This used to ICE as the peephole was not checking to see
   if the register is a floating point one (I think this cannot
   happen in real life except in this example).  */

register volatile double t1 __asm__("r14");
register volatile double t2 __asm__("r15");
register volatile double t3 __asm__("r16"), t4 __asm__("r17");
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

