/* PR c/54559 */

typedef double _Complex T;

T
foo (double x, double y)
{
  return x + y * (T) (__extension__ 1.0iF);
}
