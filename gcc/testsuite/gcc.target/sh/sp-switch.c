/* { dg-do compile }  */
/* { dg-final { scan-assembler "mov\tr0,r15" } } */
/* { dg-final { scan-assembler ".long\t\[_\]*alt_stack" } } */

void *alt_stack;
void f() __attribute__ ((interrupt_handler, sp_switch ("alt_stack")));

void f()
{
}
