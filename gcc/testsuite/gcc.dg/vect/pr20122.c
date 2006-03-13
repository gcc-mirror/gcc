/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

typedef short ashort __attribute__ ((__aligned__(16)));
ashort Kernshort[24];
static void VecBug(ashort Kernel[8][24]) __attribute__((noinline));
static void VecBug(ashort Kernel[8][24]);

/* Doesn't occur of only inner-loop. */
static void VecBug(ashort Kernel[8][24])
{
  int k,i;
    for (k = 0; k<8; k++)
        for (i = 0; i<24; i++)
            Kernshort[i] = Kernel[k][i];
}

int main (int argc, char **argv)
{
    check_vect ();

    ashort Kernel[8][24];
    int k,i;

    for (k = 0; k<8; k++)
        for (i = 0; i<24; i++)
            Kernel[k][i] = 0;

    VecBug(Kernel);

    return 0;
}


/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
