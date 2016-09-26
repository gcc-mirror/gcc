/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */

int
f (int i)
{
  switch (i)
    {
    case 0:
      i++;
      __attribute__((fallthrough));
    lab1:
    case 1:
      i++;
      __attribute__((fallthrough)); /* { dg-warning "not preceding" } */
    lab2:
      --i;
      break;
    case 3:
      i++;
      break;
    }
  return 0;
}
