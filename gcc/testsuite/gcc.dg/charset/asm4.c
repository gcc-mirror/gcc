/* Simple asm test.  */
/* { dg-do compile }
   { dg-require-iconv "IBM-1047" }
   { dg-final { scan-assembler "foo" } } */
extern int bar;

int main (void)
{
  asm ("foo %0" : "=r" (bar));
}
