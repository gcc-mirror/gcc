/* PR middle-end/38428 */

struct S
{
  volatile struct
  {
    unsigned int t : 1;
  } s;
};

int
foo (struct S *x)
{
  int ret;
  if (x->s.t)
    ret = 0;
  else
    ret = 10;
  return ret;
}
