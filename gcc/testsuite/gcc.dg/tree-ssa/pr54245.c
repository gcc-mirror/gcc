/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-slsr-details" } */

#include <stdio.h>

#define W1  22725
#define W2  21407
#define W3  19266
#define W6  8867

void idct_row(short *row, int *dst)
{
    int a0, a1, b0, b1;

    a0 = W1 * row[0];
    a1 = a0;

    a0 += W2 * row[2];
    a1 += W6 * row[2];

    b0 = W1 * row[1];
    b1 = W3 * row[1];

    dst[0] = a0 + b0;
    dst[1] = a0 - b0;
    dst[2] = a1 + b1;
    dst[3] = a1 - b1;
}

static short block[8] = { 1, 2, 3, 4 };

int main(void)
{
    int out[4];
    int i;

    idct_row(block, out);

    for (i = 0; i < 4; i++)
        printf("%d\n", out[i]);

    return !(out[2] == 87858 && out[3] == 10794);
}

/* For now, disable inserting an initializer when the multiplication will
   take place in a smaller type than originally.  This test may be deleted
   in future when this case is handled more precisely.  */
/* { dg-final { scan-tree-dump-times "Inserting initializer" 0 "slsr" } } */
/* { dg-final { cleanup-tree-dump "slsr" } } */
