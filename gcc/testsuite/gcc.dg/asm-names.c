/* The name specified by an asm("...") suffix on a declaration is not
   to have an underscore prefixed, even if normal symbols are.
   Problem reported by Krister Walfridsson <cato@df.lth.se>.  */

/* { dg-do compile } */
/* { dg-options "-fleading-underscore" } */
/* { dg-final { scan-assembler-not "____frob14" } } */

extern void frobnicate (void) asm ("___frob14");  /* three underscores */

int
main (void)
{
  frobnicate ();
  return 0;
}
