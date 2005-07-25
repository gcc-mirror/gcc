/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

typedef short ashort __attribute__ ((__aligned__(16)));
ashort Kernshort[24];
static void VecBug(ashort Kernel[8][24]) __attribute__((noinline));
static void VecBug(ashort Kernel[8][24]);
static void VecBug2(ashort Kernel[8][24]) __attribute__((noinline));
static void VecBug2(ashort Kernel[8][24]);

/* Not vectorizable: Kernel may alias Kernshort - a global array.  */
static void VecBug(ashort Kernel[8][24])
{
  int k,i;
    for (k = 0; k<8; k++)
        for (i = 0; i<24; i++)
            Kernshort[i] = Kernel[k][i];
}

/* Vectorizable: Kernshort2 is local.  */
static void VecBug2(ashort Kernel[8][24])
{
  int k,i;
  ashort Kernshort2[24];
    for (k = 0; k<8; k++)
        for (i = 0; i<24; i++)
            Kernshort2[i] = Kernel[k][i];

    for (k = 0; k<8; k++)
        for (i = 0; i<24; i++)
            if (Kernshort2[i] != Kernel[k][i])
                abort ();
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
    VecBug2(Kernel);

    return 0;
}


/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
