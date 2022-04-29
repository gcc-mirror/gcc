/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target dfp } */
/* { dg-additional-options "-fsanitize-coverage=trace-pc -fnon-call-exceptions --param=max-cse-insns=1 -frounding-math" } */
/* { dg-additional-options "-mstack-arg-probe" { target x86_64-*-* i?86-*-* } } */

void baz (int *);
void bar (double, double, _Decimal64);

void
foo (void)
{
  int s __attribute__((cleanup (baz)));
  bar (0xfffffffffffffffe, 0xebf3fff2fbebaf7f, 0xffffffffffffff);
}
