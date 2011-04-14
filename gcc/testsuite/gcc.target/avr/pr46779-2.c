/* { dg-do run } */
/* { dg-options "-Os -fno-split-wide-types" } */

/* This testcase should uncover bugs like
   PR46779
   PR45291
   PR41894

   The inline asm just serves to direct y into the Y register.
   Otherwise, it is hard to write a "stable" test case that
   also fails with slight variations in source code, middle- resp.
   backend.

   The problem is that Y is also the frame-pointer, and
   avr.c:avr_hard_regno_mode_ok disallows QI to get in Y-reg.
   However, the y.a = 0 generates a
       (set (subreg:QI (reg:HI pseudo)) ...)
   where pseudo gets allocated to Y.

   Reload fails to generate the right spill.
*/

#include <stdlib.h>

struct S
{
    unsigned char a, b;
} ab = {12, 34};

void yoo (struct S y)
{
    __asm volatile ("ldi %B0, 56" : "+y" (y));
    y.a = 0;
    __asm volatile ("; y = %0" : "+y" (y));
    ab = y;
}

int main ()
{
    yoo (ab);

    if (ab.a != 0)
        abort();

    if (ab.b != 56)
        abort();

    exit (0);
    
    return 0;
}
