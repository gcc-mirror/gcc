/* { dg-do compile { target { *-*-linux* && tls_get_addr_via_got } } } */
/* { dg-options "-fpic -fno-plt" } */

extern __thread int gd;

int *
get_gd (void)
{
  return &gd;
}

void
set_gd (int i)
{
  gd = i;
}

int
test_gd (int i)
{
  return gd == i;
}

/* { dg-final { scan-assembler "call\[ \t\]\\*__tls_get_addr@GOTPCREL" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "call\[ \t\]\\*___tls_get_addr@GOT\\(" { target ia32 } } } */
/* { dg-final { scan-assembler-not "call\[ \t\]__tls_get_addr@PLT" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "call\[ \t\]___tls_get_addr@PLT" { target ia32 } } } */
