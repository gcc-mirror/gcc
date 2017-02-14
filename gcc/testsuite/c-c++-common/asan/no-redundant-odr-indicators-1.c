/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

/* Local variables should not have odr indicators.  */
static int a = 2;
/* Thread local variables should not have odr indicators.  */
__thread int b = 3;
/* Externally visible  variables should have odr indicators.  */
int c = 1;

int main () {
    return 0;
}

/* { dg-final { scan-assembler-not "odr_asan\[\.\$\]a" } } */
/* { dg-final { scan-assembler-not "odr_asan\[\.\$\]b" } } */
/* { dg-final { scan-assembler "odr_asan\[\.\$\]c" } } */
