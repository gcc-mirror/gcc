/* PR target/65523 */
/* { dg-do compile } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */

void *memmove ();

void *
bar ()
{
  return memmove ();
}
