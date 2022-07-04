/* { dg-do compile } */
/* { dg-options "-march=rv64i -mabi=lp64 -O" } */

int test_soft_compare (_Float16 a, _Float16 b)
{
    /* Make sure __gthf2 not invoked here. */
    /* { dg-final { scan-assembler-times "call\t__extendhfsf2" 2 } } */
    return a > b;
    /* { dg-final { scan-assembler-not "call\t__gthf2" } } */
    /* { dg-final { scan-assembler-times "call\t__gtsf2" 1 } } */
}

