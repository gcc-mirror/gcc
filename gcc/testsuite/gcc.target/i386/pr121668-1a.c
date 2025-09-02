/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-Og -g -fpic -fplt -mtls-dialect=gnu" } */

typedef int caml_domain_state;
thread_local caml_domain_state caml_state;
void
caml_empty_mark_stack ()
{
  while (caml_state)
    caml_state = 0;
}

/* { dg-final { scan-assembler-times "call\[ \t\]__tls_get_addr@PLT" 1 { target { ! ia32 } } } } */
