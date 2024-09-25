/* { dg-do compile { target { c || c++11 } } } */
void f(void)
{
  [[gnu::musttail]] return; /* { dg-error "cannot tail-call.*return value must be a call" } */
}
