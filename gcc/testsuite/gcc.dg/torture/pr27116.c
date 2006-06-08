/* { dg-do run } */

extern void abort(void);

int f(int a, int b)
{
  return (-1 - a) / (-b);
}

int main()
{
  if (f(__INT_MAX__, 2) != __INT_MAX__/2 + 1)
    abort ();
  return 0;
}
