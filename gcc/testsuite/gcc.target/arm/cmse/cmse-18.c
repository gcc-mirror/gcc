/* { dg-do compile } */
/* { dg-options "-mcmse -fdump-rtl-final-slim" } */

typedef void (*f)(int) __attribute__((cmse_nonsecure_call));

void bar(f func, int a)
{
  func(a);
}

/* { dg-final { scan-rtl-dump "call unspec\\\[\\\[r4:SI\\\]\\\]" "final" { target { ! arm_v8_1m_mve_ok } } } } */
/* { dg-final { scan-rtl-dump "call unspec\\\[\\\[r\[0-7\]:SI\\\]\\\]" "final" { target { arm_v8_1m_mve_ok } } } } */
