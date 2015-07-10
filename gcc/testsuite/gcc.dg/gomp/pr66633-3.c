/* PR middle-end/66633 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -O1" } */

void baz (int (*) ());

void
foo (void)
{
  int i;
  auto int bar (void) { return i; }
  auto void bar2 (void)
  {
    #pragma omp parallel
      baz (bar);
  }
  bar2 ();
}
