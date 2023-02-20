/* PR rtl-optimization/103541 */
/* { dg-do compile  { target { ! ia32 } } } */
/* { dg-options "-O2" } */

float a;
__attribute__((const)) float foo (float);

float
test()
{
        return a + foo(a) + a;
}

/* { dg-final { scan-assembler-not "\\\(%rsp\\\)" } } */
