/* PR target/56809 */
/* { dg-do compile } */
/* { dg-options "-Os" }  */

int
foo (int mode, int i)
{
  int x;

  switch (mode)
    {
    case 0:
      x = i + 1;
      break;
    case 1:
      x = i / 2;
      break;
    case 2:
      x = i * 3;
      break;
    case 3:
      x = i + 3;
      break;
    case 4:
      x = i + 5;
      break;
    default:
      x = i - 1;
    }

  return x;
}
