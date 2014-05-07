/* PR tree-optimization/58209 */

extern void abort (void);
typedef __INTPTR_TYPE__ T;
T buf[1024];

T *
foo (T n)
{
  if (n == 0)
    return (T *) buf;
  T s = (T) foo (n - 1);
  return (T *) (s + sizeof (T));
}

T *
bar (T n)
{
  if (n == 0)
    return buf;
  return foo (n - 1) + 1;
}

int
main ()
{
  int i;
  for (i = 0; i < 27; i++)
    if (foo (i) != buf + i || bar (i) != buf + i)
      abort ();
  return 0;
}
