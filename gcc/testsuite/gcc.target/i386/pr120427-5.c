/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-Oz" } */

long long
func1 (void)
{
  return -1;
}
/* { dg-final { scan-assembler-times "pushq\[ \\t\]+\\\$-1" 1 } } */
/* { dg-final { scan-assembler-times "popq\[ \\t\]+%rax" 1 } } */
