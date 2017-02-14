/* PR c/64279 */
/* { dg-do compile } */
/* { dg-options "-Wduplicated-branches" } */

#define DEBUG(msg) ;

void
f (int i)
{
  if (i > 9)
    {
      DEBUG ("foo");
    }
  else
    {
      DEBUG ("bar");
    }
}
