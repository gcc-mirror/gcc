/* PR target/79633 */
/* { dg-do compile } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -w -O2" } */

extern void *memcpy ();

void
foo ()
{
  memcpy ();
}
