/* The name specified by an asm("...") suffix on a declaration is not
   to have an underscore prefixed, even if normal symbols are.
   Problem reported by Krister Walfridsson <cato@df.lth.se>.  */

/* { dg-do link } */
/* { dg-options "-fleading-underscore" } */

extern void frobnicate (void) asm ("___frob14");  /* three underscores */

void __frob14 (void) {} /* two underscores */

int
main (void)
{
  frobnicate ();
  return 0;
}

/* In case built where the runtime expects no leading underscore on
   main(). */
extern int xmain (void) asm ("main");

int xmain (void) { return main(); }

/* In case built where the runtime calls __main.  */
extern int ymain (void) asm ("___main");
int ymain (void) { return main(); }
