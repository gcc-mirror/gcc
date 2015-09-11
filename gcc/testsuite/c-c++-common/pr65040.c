/* PR c/65040 */
/* { dg-do compile } */
/* { dg-options "-Wformat -Wformat-signedness" } */

unsigned char uc;
signed char sc;
unsigned short us;
signed short ss;
unsigned int u;
int i;

void
foo (void)
{
  __builtin_printf ("%u\n", uc); /* { dg-bogus "expects argument of type" } */
  __builtin_printf ("%u\n", sc); /* { dg-warning "expects argument of type" } */
  __builtin_printf ("%u\n", us); /* { dg-bogus "expects argument of type" } */
  __builtin_printf ("%u\n", ss); /* { dg-warning "expects argument of type" } */
  __builtin_printf ("%u\n", u); /* { dg-bogus "expects argument of type" } */
  __builtin_printf ("%u\n", i); /* { dg-warning "expects argument of type" } */
}
