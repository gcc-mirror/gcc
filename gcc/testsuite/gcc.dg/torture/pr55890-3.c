/* { dg-do compile } */

void *memmove ();

void *
bar ()
{
  return memmove ();
}
