/* PR c++/98742 */
/* { dg-do compile } */

void
foo ()
{
#pragma omp task detach(0)	/* { dg-error "before numeric constant" } */
  ;
}
