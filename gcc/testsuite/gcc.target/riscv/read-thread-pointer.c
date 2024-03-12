/* { dg-do compile } */
/* { dg-require-effective-target tls_native } */

void *get_tp()
{
    return __builtin_thread_pointer ();
}
/* { dg-final { scan-assembler "mv\[ \t\]*\[at\]\[0-9\]+,tp" } } */
