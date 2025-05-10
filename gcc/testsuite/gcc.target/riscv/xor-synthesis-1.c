/* { dg-do compile { target { rv64 } } } */
/* { dg-options "-march=rv64gb -mabi=lp64d" } */

unsigned long foo(unsigned long src) { return src ^ 0xffffffffefffffffUL; }

/* { dg-final { scan-assembler-times "\\sbinvi\t" 1 } } */
/* { dg-final { scan-assembler-times "\\snot\t" 1 } } */

