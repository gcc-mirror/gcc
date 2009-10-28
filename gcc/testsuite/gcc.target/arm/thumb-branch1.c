/* { dg-do compile } */
/* { dg-options "-Os -mthumb -march=armv5te" } */

int returnbool(int a, int b)
{
    if (a < b)
        return 1;
    return 0;
}

/* { dg-final { scan-assembler-not "eor" } } */
