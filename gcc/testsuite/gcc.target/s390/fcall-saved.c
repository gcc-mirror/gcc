/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -fcall-saved-r4" } */

void test(void) {
    asm volatile("nop" ::: "r4");
}

/* { dg-final { scan-assembler-times "\tstg\t" 1 { target { lp64 } } } } */
/* { dg-final { scan-assembler-times "\tlg\t" 1 { target { lp64 } } } } */
/* { dg-final { scan-assembler-times "\tst\t" 1 { target { ! lp64 } } } } */
/* { dg-final { scan-assembler-times "\tl\t" 1 { target { ! lp64 } } } } */
