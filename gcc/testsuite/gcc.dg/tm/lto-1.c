/* { dg-do compile } */
/* { dg-options "-fgnu-tm -flto" } */

__attribute__((transaction_safe))
void foo() 
{
}

/* { dg-final { scan-assembler "ZGTt3foo" } } */
