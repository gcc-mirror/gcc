/* PR target/94913 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-pie" } */

void f1 (void);
void f2 (void);

void fooc (unsigned char x, unsigned char y)
{
  if ((unsigned char) ~x < y) f1 (); else f2 ();
}

void foos (unsigned short x, unsigned short y)
{
  if ((unsigned short) ~x < y) f1 (); else f2 ();
}

void fooi (unsigned long x, unsigned long y)
{
  if ((unsigned long) ~x < y) f1 (); else f2 ();
}

/* { dg-final { scan-assembler-not "cmp" } } */
/* On IA32, PIC/PIE adds one add per function to compute the PIC register, and
   another add to adjust %esp in the epilogue needed to restore the PIC
   register, but the epilogue may or may not be duplicated.  Avoid this
   complexity by disabling PIC/PIE altogether.  */
/* { dg-final { scan-assembler-times "add" 3 } } */
