/* { dg-options "-mxl-gp-opt -G 16" } */

struct test_s {
    int a;
    int b;
    int c;
    int d;
};

/* { dg-final { scan-assembler "\.sdata2" } } */
const struct test_s global1 = { 1, 2, 3, 4};
extern const struct test_s global2;

int testfunc ()
{
/* { dg-final { scan-assembler "\lwi\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r2" } } */
    return global2.a + global1.a;
}
