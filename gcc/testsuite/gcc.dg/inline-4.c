/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "big_static_inline" } } */

extern void f(void);
static inline void big_static_inline(void)
{
  f(); f(); f(); f(); f(); f(); f(); f(); f(); f();
  f(); f(); f(); f(); f(); f(); f(); f(); f(); f();
  f(); f(); f(); f(); f(); f(); f(); f(); f(); f();
  f(); f(); f(); f(); f(); f(); f(); f(); f(); f();
  f(); f(); f(); f(); f(); f(); f(); f(); f(); f();
  f(); f(); f(); f(); f(); f(); f(); f(); f(); f();
  f(); f(); f(); f(); f(); f(); f(); f(); f(); f();
  f(); f(); f(); f(); f(); f(); f(); f(); f(); f();
  f(); f(); f(); f(); f(); f(); f(); f(); f(); f();
  f(); f(); f(); f(); f(); f(); f(); f(); f(); f();
}
