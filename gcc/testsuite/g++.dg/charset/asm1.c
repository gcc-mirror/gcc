/* { dg-do compile { target *-*-* } }
   { dg-require-iconv "IBM1047" }
   { dg-final { scan-assembler ".ascii bar" } }
   { dg-final { scan-assembler ".ascii foo" } }
 */
extern int x, y;

asm (".ascii bar");

int foo (void)
{
  __asm__ (".ascii foo");
  return 0;
}
