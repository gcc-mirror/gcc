/* PR c/48517 */
/* { dg-do compile } */
/* { dg-options "" } */

void bar (const unsigned short *);

void
foo (void)
{
  static const unsigned short array[] = (const unsigned short []) { 0x0D2B };
  const unsigned short *ptr = array;
  bar (ptr);
}
