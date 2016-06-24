/* { dg-do compile { target { *-*-linux* && tls_get_addr_via_got } } } */
/* { dg-options "-fpic -fno-plt" } */

static __thread int ld;

int *
get_ld (void)
{
  return &ld;
}

void
set_ld (int i)
{
  ld = i;
}

int
test_ld (int i)
{
  return ld == i;
}

/* { dg-final { scan-assembler "call\[ \t\]\\*__tls_get_addr@GOTPCREL" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "call\[ \t\]\\*___tls_get_addr@GOT\\(" { target ia32 } } } */
/* { dg-final { scan-assembler-not "call\[ \t\]__tls_get_addr@PLT" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "call\[ \t\]___tls_get_addr@PLT" { target ia32 } } } */
