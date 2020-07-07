/* { dg-do compile } */

void *get_tp()
{
    return __builtin_thread_pointer ();
}
/* { dg-final { scan-assembler "mv\[ \t\]*[at][0-9]+,tp" } } */
