/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zicbop" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zicbop" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

void test1() { __builtin_prefetch((int *)2047); }
void test2() { __builtin_prefetch((int *)1024); }
void test3(char *x) { __builtin_prefetch(&x); }
void test4(char *x) { __builtin_prefetch(&x[2]); }
void test5(char *x) { __builtin_prefetch(&x[1024]); }

/* So we expect test1, test3 and test4 to be a prefetch
   with zero offset.  test2 and test5 will have a 1k offset.  */
/* { dg-final { scan-assembler-times "prefetch.r\t0\\(\[a-x0-9\]+\\)" 3 } } */
/* { dg-final { scan-assembler-times "prefetch.r\t1024" 2 } } */

