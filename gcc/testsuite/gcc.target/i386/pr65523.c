/* PR target/65523 */
/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */

void *memmove ();

void *
bar ()
{
  return memmove ();
}
