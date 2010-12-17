/* { dg-options "-mxl-gp-opt" } */

/* { dg-final { scan-assembler "\.sdata\[^2]+" } } */
int global = 10;

int testfunc ()
{
/* { dg-final { scan-assembler "\lwi\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r13" } } */
    return global;
}
