/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* { dg-require-effective-target ia32 } */
/* { dg-final { scan-assembler "movl\t\\\$6700417, %eax" } } */

unsigned ud_x_641_mul(unsigned x) {
    /* optimized version of x / 641 */
    return ((unsigned long long)x * 0x663d81) >> 32;
}
