/* Test for resbank attribute.  */
/* { dg-do assemble {target sh*-*-*}} */
/* { dg-skip-if "" { "sh*-*-*" } "*" "-m2a -m2a-nofpu -m2a-single -m2a-single-only" } */
/* { dg-final { scan-assembler "resbank" } } */
 
extern void bar(void);
 
void foo(void) __attribute__((interrupt_handler, resbank));
void foo(void)
{
  bar();
}
