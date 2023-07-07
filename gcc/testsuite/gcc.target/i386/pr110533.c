/* { dg-do compile { target int128 } } */
/* { dg-options "-O0" } */

__attribute__((naked))
void fn(__int128 a) {
    asm("ret");
}

/* { dg-final { scan-assembler-not "mov" } } */
