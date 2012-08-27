/* { dg-do compile } */
/* { dg-options "-mno-long-calls addressing=absolute" } */

NOMIPS16 extern int long_call_func () __attribute__((long_call));
NOMIPS16 extern int far_func () __attribute__((far));
NOMIPS16 extern int near_func () __attribute__((near));
NOMIPS16 extern int normal_func ();

NOMIPS16 int test1 () { return long_call_func (); }
NOMIPS16 int test2 () { return far_func (); }
NOMIPS16 int test3 () { return near_func (); }
NOMIPS16 int test4 () { return normal_func (); }

/* { dg-final { scan-assembler-not "\tj\tlong_call_func\n" } } */
/* { dg-final { scan-assembler-not "\tj\tfar_func\n" } } */
/* { dg-final { scan-assembler     "\tj(|al)\tnear_func\n" } } */
/* { dg-final { scan-assembler     "\tj(|al)\tnormal_func\n" } } */
