/* Test error conditions of asm flag outputs.  */
/* { dg-do compile } */
/* { dg-options "" } */

void f_B(void) { _Bool x; asm("" : "=@ccc"(x)); }
void f_c(void) { char x; asm("" : "=@ccc"(x)); }
void f_s(void) { short x; asm("" : "=@ccc"(x)); }
void f_i(void) { int x; asm("" : "=@ccc"(x)); }
void f_l(void) { long x; asm("" : "=@ccc"(x)); }
void f_ll(void) { long long x; asm("" : "=@ccc"(x)); }

void f_f(void)
{
  float x;
  asm("" : "=@ccc"(x)); /* { dg-error invalid type } */
}

void f_d(void)
{
  double x;
  asm("" : "=@ccc"(x)); /* { dg-error invalid type } */
}

struct S { int x[3]; };

void f_S(void)
{
  struct S x;
  asm("" : "=@ccc"(x)); /* { dg-error invalid type } */
}
