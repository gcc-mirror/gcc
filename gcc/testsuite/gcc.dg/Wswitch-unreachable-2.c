/* PR middle-end/71476 */
/* { dg-do compile } */
/* { dg-options "-Wswitch-unreachable" } */

void
foo (int a)
{
  switch (a)
    {
      void f (void) { }
    }
}
