/* Test error conditions of asm flag outputs.  */
/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-skip-if "" { arm_thumb1 } } */

void f_B(void) { _Bool x; asm("" : "=@cccc"(x)); }
void f_c(void) { char x; asm("" : "=@cccc"(x)); }
void f_s(void) { short x; asm("" : "=@cccc"(x)); }
void f_i(void) { int x; asm("" : "=@cccc"(x)); }
void f_l(void) { long x; asm("" : "=@cccc"(x)); }
void f_ll(void) { long long x; asm("" : "=@cccc"(x)); }

void f_f(void)
{
  float x;
  asm("" : "=@cccc"(x)); /* { dg-error "invalid type" } */
}

void f_d(void)
{
  double x;
  asm("" : "=@cccc"(x)); /* { dg-error "invalid type" } */
}

struct S { int x[3]; };

void f_S(void)
{
  struct S x;
  asm("" : "=@cccc"(x)); /* { dg-error "invalid type" } */
}
