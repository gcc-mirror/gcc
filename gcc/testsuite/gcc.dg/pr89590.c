/* PR middle-end/89590 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wall -w" } */

void free (void *);

void
foo (void)
{
  ((void (*)()) free) ();
}
