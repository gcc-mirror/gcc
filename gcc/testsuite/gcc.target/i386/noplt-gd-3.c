/* { dg-do compile { target { *-*-linux* && { ia32 && tls_get_addr_via_got } } } } */
/* { dg-options "-fpic -fno-plt" } */

extern __thread int gd;

int *
get_gd (void)
{
  return &gd;
}

/* { dg-final { scan-assembler-not "call\[ \t\]\\*___tls_get_addr@GOT\\(%ebx\\)" } } */
