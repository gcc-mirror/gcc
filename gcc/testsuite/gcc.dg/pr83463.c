/* PR middle-end/83463 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wrestrict -Wno-pointer-to-int-cast" } */

int *a;
void *memcpy ();
void
m (void *p1)
{
  memcpy (0, p1, 0);
}

void
p ()
{
  m (p + (long) a);
}
