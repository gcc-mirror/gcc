/* { dg-do compile { target { *-*-linux* && { ia32 && tls_get_addr_via_got } } } } */
/* { dg-options "-fpic -fno-plt" } */

static __thread int ld;

int *
get_ld (void)
{
  return &ld;
}

/* { dg-final { scan-assembler-not "call\[ \t\]\\*___tls_get_addr@GOT\\(%ebx\\)" } } */
