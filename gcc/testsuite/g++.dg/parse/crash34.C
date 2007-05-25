/* PR c++/31745 */
/* { dg-do "compile" }  */

void foo()
{
  namespace N { /* { dg-error "is not allowed|at end of input" } */
