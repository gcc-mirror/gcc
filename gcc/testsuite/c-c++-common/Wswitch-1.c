/* PR c++/105497 */
/* { dg-options "-Wswitch" } */

enum E {
  A,
  B,
  C __attribute((unused)),
  D
};

void
g (enum E e)
{
  switch (e)
    {
    case A:
    case B:
    case D:
      break;
    }

  switch (e) // { dg-warning "not handled in switch" }
    {
    case A:
    case B:
    case C:
      break;
    }
}
