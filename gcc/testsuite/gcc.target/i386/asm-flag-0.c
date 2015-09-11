/* Test error conditions of asm flag outputs.  */
/* { dg-do compile } */
/* { dg-options "" } */

void a(void)
{
  char x;
  asm("" : "=@cca,@ccc"(x));  /* { dg-error "alternatives not allowed" } */
}

void b(void)
{
  char x;
  asm("" : "=@ccbad"(x)); /* { dg-error "unknown asm flag output" } */
}
