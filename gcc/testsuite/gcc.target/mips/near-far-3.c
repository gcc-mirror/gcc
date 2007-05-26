/* { dg-do compile } */
/* { dg-mips-options "-mlong-calls -O2 -mno-mips16" } */
/* { dg-require-effective-target nonpic } */

extern int long_call_func () __attribute__((long_call));
extern int far_func () __attribute__((far));
extern int near_func () __attribute__((near));
extern int normal_func ();

int test1 () { return long_call_func (); }
int test2 () { return far_func (); }
int test3 () { return near_func (); }
int test4 () { return normal_func (); }

/* { dg-final { scan-assembler-not "\tj\tlong_call_func\n" } } */
/* { dg-final { scan-assembler-not "\tj\tfar_func\n" } } */
/* { dg-final { scan-assembler     "\tj\tnear_func\n" } } */
/* { dg-final { scan-assembler-not "\tj\tnormal_func\n" } } */
