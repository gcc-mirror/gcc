/* { dg-do run } */
/* { dg-require-effective-target nonlocal_goto } */
/* { dg-require-effective-target scheduling } */
/* { dg-options "-O2 -fschedule-insns" } */

#include <stdio.h>
#include <setjmp.h>

jmp_buf ex_buf;

__attribute__((noipa))
void fn_throw(int x)
{
   if (x)
      longjmp(ex_buf, 1);
}

int main(void)
{
    int vb = 0; // NB: not volatile, not modified after setjmp

    if (!setjmp(ex_buf)) {
        fn_throw(1);
        vb = 1; // not reached in the abstract machine
    }

    if (vb) {
        printf("Failed, vb = %d!\n", vb);
        return 1;
    }
}
