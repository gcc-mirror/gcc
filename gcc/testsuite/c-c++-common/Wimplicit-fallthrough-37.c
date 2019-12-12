/* PR c++/87068 */
/* { dg-do compile } */

void
f (int n)
{
  switch (n)
    {
    case 4:
      ++n;
      __attribute__((fallthrough)); /* { dg-warning "not preceding" } */
    }
}
