/* { dg-options "-mxl-gp-opt -G 16 -fno-pic" } */

/* { dg-final { scan-assembler "\.sbss\[^2]+" } } */
struct test_s {
    int a;
    int b;
    int c;
    int d;
} global; 

int testfunc ()
{
/* { dg-final { scan-assembler "\lwi\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r13" } } */
    return global.a;
}
