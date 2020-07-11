/* PR target/94913 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

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
/* { dg-final { scan-assembler-times "add" 3 } } */
