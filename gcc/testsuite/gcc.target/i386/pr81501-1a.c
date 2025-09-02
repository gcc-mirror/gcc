/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -fplt -mtls-dialect=gnu" } */

void a(long *);
int b(void);
void c(void);
static __thread long e;
long
d(void)
{
  a(&e);
  if (b())
    c();
  return e;
}

/* { dg-final { scan-assembler-times "call\[ \t\]__tls_get_addr@PLT" 1 { target { ! ia32 } } } } */
