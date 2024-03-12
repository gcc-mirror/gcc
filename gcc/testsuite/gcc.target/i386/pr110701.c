/* { dg-do compile } */
/* { dg-options "-O2" } */
int a;
long b;
int *c = &a;
short d(short e, short f) { return e * f; }
void foo() {
  *c = d(340, b >= 0) ^ 3;
}

/* { dg-final { scan-assembler "andl\[ \\t]\\\$340," } } */
/* { dg-final { scan-assembler-not "andw\[ \\t]\\\$340," } } */
