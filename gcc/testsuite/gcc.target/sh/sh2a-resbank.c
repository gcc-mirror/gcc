/* Test for resbank attribute.  */
/* { dg-do compile { target { sh2a } } }  */
/* { dg-final { scan-assembler "resbank" } } */
 
extern void bar(void);
 
void foo(void) __attribute__((interrupt_handler, resbank));
void foo(void)
{
  bar();
}
