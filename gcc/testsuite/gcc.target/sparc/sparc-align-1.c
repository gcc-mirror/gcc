/* PR target/31100 */
/* Reported by Erwin Unruh <Erwin.Unruh@fujitsu-siemens.com> */

/* { dg-do run } */
/* { dg-options "-falign-labels=16" } */

extern void abort(void);

int f(int i)
{
  int res;

  switch (i)
    {
    case 5:
      res = i - i;
      break;
    default:
      res = i * 2;
      break;
    }

  return res;
}

int main(void)
{
  if (f(2) != 4)
    abort ();
  return 0;
}
