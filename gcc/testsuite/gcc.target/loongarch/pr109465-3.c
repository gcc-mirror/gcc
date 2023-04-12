/* { dg-do compile } */
/* { dg-options "-O2 -mabi=lp64d -mstrict-align" } */

/* Three loop iterations each contains 4 st.b, and 3 st.b after the loop */
/* { dg-final { scan-assembler-times "st\\.b" 7 } } */

/* { dg-final { scan-assembler-not "st\\.h" } } */
/* { dg-final { scan-assembler-not "st\\.w|stptr\\.w" } } */
/* { dg-final { scan-assembler-not "st\\.d|stptr\\.d" } } */

extern char a[], b[];
void test() { __builtin_memcpy(a, b, 15); }
