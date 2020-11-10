/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */

int
f (int i)
{
  switch (i)
    {
    case -1:
      __attribute__((fallthrough));
    default:
      __attribute__((fallthrough));
    case 1:
      return 6;
    case 2 ... 4:
      __attribute__((fallthrough));
    case 5:
      return 7;
    }
  return 0;
}

int
g (int i)
{
  switch (i)
    {
    case -1:
      __attribute__((used)); /* { dg-warning "empty declaration|ignored" } */
    default:
      __attribute__((used)); /* { dg-warning "empty declaration|ignored" } */
    case 1:
      return 6;
    case 2 ... 4:
      __attribute__((used)); /* { dg-warning "empty declaration|ignored" } */
    case 5:
      return 7;
    }
  return 0;
}
