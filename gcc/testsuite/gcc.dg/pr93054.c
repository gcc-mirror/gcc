/* { dg-do compile } */

__attribute__ ((returns_twice)) int
bp (int);

__attribute__ ((noreturn)) int
cb (void)
{
  return bp (cb ()); /* { dg-warning "declared .noreturn." } */
}
