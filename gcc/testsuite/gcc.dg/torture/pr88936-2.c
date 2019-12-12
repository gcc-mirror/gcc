/* { dg-do run } */
/* { dg-additional-options "-fipa-pta" } */

static int *p;
void bar(int cnt)
{
  int i = 0;
  if (cnt == 0)
    {
      p = &i;
      bar (1);
      if (i != 1)
	__builtin_abort ();
    }
  else if (cnt == 1)
    *p = 1;
}
int main()
{
  bar (0);
  return 0;
}
