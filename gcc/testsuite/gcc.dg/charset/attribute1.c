/* Test for attribute non-translation.  */
/* { dg-do compile }
   { dg-require-iconv "IBM1047" }
   { dg-final { scan-assembler "foo" } } */
int walrus __attribute__ ((section (".foo")));

int main (void)
{
  return 0;
}
