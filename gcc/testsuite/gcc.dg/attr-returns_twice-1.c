/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-W" } */

int newsetjmp(void) __attribute__((returns_twice));
void g(int);

int
main (void)
{
  register int reg asm ("esi") = 1; /* { dg-warning "might be clobbered" "" } */

  if (!newsetjmp ())
    {
      reg = 2;
      g (reg);
    }
  else
    {
      g (reg);
    }

  return 0;
}
