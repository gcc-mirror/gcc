/* { dg-do compile } */
/* { dg-options "-march=rv64if -mabi=lp64f -O" } */

_Float16 test_soft_add (_Float16 a, _Float16 b)
{
    /* Make sure __addhf3 not invoked here. */
    /* { dg-final { scan-assembler-times "call\t__extendhfsf2" 2 } } */
    return a + b;
    /* { dg-final { scan-assembler-not "call\t__addhf3" } } */
    /* { dg-final { scan-assembler-times "fadd.s" 1 } } */
    /* { dg-final { scan-assembler-times "call\t__truncsfhf2" 1 } } */
}

