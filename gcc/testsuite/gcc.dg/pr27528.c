/* Check the warnings and errors generated for asm operands that aren't
   obviously constant but that are constrained to be constants.  */
/* { dg-options "" } */
/* { dg-error "impossible constraint" "" { target *-*-* } 13 } */
/* { dg-error "impossible constraint" "" { target *-*-* } 14 } */
/* { dg-error "impossible constraint" "" { target *-*-* } 15 } */
/* { dg-error "impossible constraint" "" { target *-*-* } 16 } */
int bar (int);
void
foo (int *x, int y)
{
  int constant = 0;
  asm ("# %0" :: "i" (x)); /* { dg-warning "probably doesn't match" } */
  asm ("# %0" :: "i" (bar (*x))); /* { dg-warning "probably doesn't match" } */
  asm ("# %0" :: "i" (*x + 0x11)); /* { dg-warning "probably doesn't match" } */
  asm ("# %0" :: "i" (constant)); /* { dg-warning "probably doesn't match" } */
  asm ("# %0" :: "i" (y * 0)); /* folded */
}
