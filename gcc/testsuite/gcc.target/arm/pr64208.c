/* { dg-do compile } */
/* { dg-skip-if "Test is specific to the iWMMXt" { arm*-*-* } { "-mcpu=*" } { "-mcpu=iwmmxt" } } */
/* { dg-skip-if "Test is specific to the iWMMXt" { arm*-*-* } { "-mabi=*" } { "-mabi=iwmmxt" } } */
/* { dg-skip-if "Test is specific to the iWMMXt" { arm*-*-* } { "-march=*" } { "-march=iwmmxt" } } */
/* { dg-skip-if "Test is specific to ARM mode" { arm*-*-* } { "-mthumb" } { "" } } */
/* { dg-require-effective-target arm32 } */
/* { dg-require-effective-target arm_iwmmxt_ok } */
/* { dg-options "-O1 -mcpu=iwmmxt" } */

long long x6(void);
void x7(long long, long long);
void x8(long long);

int x0;
long long *x1;

void x2(void) {
  long long *x3 = x1;
  while (x1) {
    long long x4 = x0, x5 = x6();
    x7(x4, x5);
    x8(x5);
    *x3 = 0;
  }
}
