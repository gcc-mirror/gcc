/* { dg-options "-mxl-gp-opt -fno-pic" } */

extern int a;

/* { dg-final { scan-assembler "\.sdata2" } } */
const int global1 = 10;
extern const int global2;

int testfunc ()
{
/* { dg-final { scan-assembler "\lwi\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r2" } } */
    return global2 + global1;
}
