/* Check that eliminable compare-instructions are eliminated. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tcmp|\ttest" } } */

#ifndef t
#define t short int
#endif
#ifndef t2
#define t2 t
#endif
#ifndef op
#define op +
#endif
#ifndef do_f
#define do_f 1
#endif
#ifndef do_g
#define do_g 1
#endif

extern void foo(void);

#if do_f
void f(t a, t b)
{
  t2 c = a op b;

  if (c == 0)
    foo();
}
#endif

#if do_g
void g(t a, t b)
{
  t2 c = a op b;

  if (c >= 0)
    foo();
}
#endif
