/* Test generation of DFP instructions for POWER6.  */
/* Origin: Janis Johnson <janis187@us.ibm.com> */
/* { dg-do compile { target { powerpc*-*-linux* && powerpc_fprs } } } */
/* { dg-options "-std=gnu99 -mdejagnu-cpu=power6" } */

/* { dg-final { scan-assembler "dadd" } } */
/* { dg-final { scan-assembler "ddiv" } } */
/* { dg-final { scan-assembler "dmul" } } */
/* { dg-final { scan-assembler "dsub" } } */
/* { dg-final { scan-assembler-times "dcmpu" 6 } } */
/* { dg-final { scan-assembler-times "dctfix" 2 } } */
/* { dg-final { scan-assembler-times "drintn" 2 } } */
/* { dg-final { scan-assembler-times "dcffixq" 2 } } */

extern _Decimal64 a, b, c;
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
void ddsi (void) { si = a; }
void dddi (void) { di = a; }
void sidd (void) { a = si; }
void didd (void) { a = di; }
