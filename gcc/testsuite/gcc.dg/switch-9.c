/* PR middle-end/18859 */
/* { dg-do compile } */
/* { dg-options "" } */

void foo(int a)
{
  switch (a)
  {
    case 0 ... -1:  /* { dg-warning "empty range" } */
      a = a+2;
      break;

    case 1 ... 2:
      a = 0;
      break;

    case 3 ... 4:
      a = 1;
      break;

    case 5 ... 6:
      a = 0;
      break;
  }
}
