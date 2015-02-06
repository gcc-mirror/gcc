/* { dg-do compile }  */
/* { dg-options "-mh" }  */
/* { dg-final { scan-assembler-times "@@" 1 } }  */


void foo (void) __attribute__ ((function_vector));
__attribute__((noinline)) void foo (void)
{
}

void bar (void)
{
    foo();
}
