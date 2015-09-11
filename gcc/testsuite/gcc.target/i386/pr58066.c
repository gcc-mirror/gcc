/* { dg-do compile } */
/* { dg-require-effective-target tls_native } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fPIC -fomit-frame-pointer -O2 -fdump-rtl-final" } */

/* Check whether the stack frame starting addresses of tls expanded calls
   in foo and goo are 16bytes aligned.  */
static __thread char ccc1;
void* foo()
{
 return &ccc1;
}

__thread char ccc2;
void* goo()
{
 return &ccc2;
}

/* { dg-final { scan-rtl-dump "Function foo.*set\[^\r\n\]*sp\\)\[\r\n\]\[^\r\n\]*plus\[^\r\n\]*sp\\)\[\r\n\]\[^\r\n\]*const_int -8.*UNSPEC_TLS.*Function goo" "final" } } */
/* { dg-final { scan-rtl-dump "Function goo.*set\[^\r\n\]*sp\\)\[\r\n\]\[^\r\n\]*plus\[^\r\n\]*sp\\)\[\r\n\]\[^\r\n\]*const_int -8.*UNSPEC_TLS" "final" } } */
