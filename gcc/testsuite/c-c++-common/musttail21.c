/* { dg-do compile { target musttail } } */
void f(void)
{
  __attribute__((musttail)) return; /* { dg-error "cannot tail-call.*return value must be a call" } */
}
