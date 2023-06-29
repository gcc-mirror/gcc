/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mavx2 -mmove-max=128 -mstore-max=128" } */

int foo(char *a)
{
    static const char t[] = "0123456789012345678901234567890";
    return __builtin_memcmp(a, &t[0], sizeof(t)) == 0;
}

/* { dg-final { scan-assembler-not "movl\[ \\t]*\\\$0," } } */
/* { dg-final { scan-assembler-not "vptest\[ \\t]*%ymm" } } */
/* { dg-final { scan-assembler-times "vptest\[ \\t]*%xmm" 2 } } */

