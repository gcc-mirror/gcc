/* PR middle-end/14730 */

int t (char i)
{
  switch (i)
    {
    case 1:
    case 7:
    case 10:
    case 14:
    case 9:
    case 256:
      return 0;
    }
  return 1;
}
