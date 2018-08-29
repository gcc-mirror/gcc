/* PR target/81639 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void b (void);

void
__attribute__ ((naked))
a (void)
{
  b ();
}
