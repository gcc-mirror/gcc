/* PR c/94686 */

int a = 0, b = 0;

int
foo (void)
{
  return (int) (long) (b * 0 >= a & b * 0 >= a);
}
