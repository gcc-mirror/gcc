/* { dg-do compile { target alpha*-*-osf5* } } */
/* { dg-final { scan-assembler ",Xfoo" } } */

#pragma extern_prefix "X"
void foo(void) __attribute__((noreturn));
void foo(void) __attribute__((noreturn));
void bar()
{
  foo();
}
