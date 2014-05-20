/* { dg-do run } */
/* { dg-options "-O1" } */

/* This testcase (simplified from the original bug report) exposes 
   PR60991. The code generated for writing the __int24 value corrupts
   the frame pointer if the offset is <= 63 + MAX_LD_OFFSET */

#include <stdlib.h>

int main(void)
{
    volatile char junk[62];
    junk[0] = 5;
    volatile __int24 staticConfig = 0;

    if (junk[0] != 5)
      abort();

    exit(0);
    return 0;
}
