/* Test generation of DFP instructions for POWER6.  */
/* Origin: Janis Johnson <janis187@us.ibm.com> */
/* { dg-do compile { target { powerpc*-*-linux* && powerpc_fprs } } } */
/* { dg-options "-std=gnu99 -mdejagnu-cpu=power6" } */

/* { dg-final { scan-assembler "daddq" } } */
/* { dg-final { scan-assembler "ddivq" } } */
/* { dg-final { scan-assembler "dmulq" } } */
/* { dg-final { scan-assembler "dsubq" } } */
/* { dg-final { scan-assembler-times "dcmpuq" 6 } } */
/* { dg-final { scan-assembler-times "dctfixq" 2 } } */
/* { dg-final { scan-assembler-times "drintnq" 2 } } */
/* { dg-final { scan-assembler-times "dcffixq" 2 } } */

extern _Decimal128 a, b, c;
extern int result;
extern int si;
extern long long di;

void add (void) { a = b + c; }
void div (void) { a = b / c; }
void mul (void) { a = b * c; }
void sub (void) { a = b - c; }
void eq (void) { result = a == b; }
void ne (void) { result = a != b; }
void lt (void) { result = a < b; }
void le (void) { result = a <= b; }
void gt (void) { result = a > b; }
void ge (void) { result = a >= b; }
void tdsi (void) { si = a; }
void tddi (void) { di = a; }
void sitd (void) { a = si; }
void ditd (void) { a = di; }
