/* PR middle-end/14203 */
/* { dg-do compile } */
/* { dg-options "-Wall" } */

void foo()
{
  if (false)
    if (int i=0)  // { dg-warning "unused" "" }
      int j=0;    // { dg-warning "unused" "" }
}

