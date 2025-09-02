/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-std=c++14 -mtls-dialect=gnu -O2 -fpic -fplt" } */
/* { dg-final { scan-assembler-times "call\[ \t\]__tls_get_addr@PLT" 1 { target { ! ia32 } } } } */

struct foo
{
  foo();
  ~foo();
};

foo *
test ()
{
  static thread_local foo foo_tls;
  return &foo_tls;
}
