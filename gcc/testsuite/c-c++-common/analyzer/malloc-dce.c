/* { dg-additional-options "-O2" } */
/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

#include <stdlib.h>

void test(void)
{
    void *ptr = malloc(512);
    free(ptr);
    free(ptr); /* { dg-warning "double-'free'" "" { xfail *-*-* } } */
}
/* With optimization, the whole of test() goes away in the "cddce" pass
   before the analysis pass sees it, and hence we get no error message.  */
