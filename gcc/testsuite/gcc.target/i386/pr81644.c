/* PR target/81644 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-mregparm=1" { target ia32 } } */

void b (void);

void
__attribute__ ((naked))
a (int z)
{
  if (z)
    return;
  b ();
}
