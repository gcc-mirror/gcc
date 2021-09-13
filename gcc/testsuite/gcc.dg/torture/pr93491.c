/* { dg-do run } */

extern void exit (int);

__attribute__((noipa))
void f(int i)
{
  exit(i);
}

__attribute__((const,noipa))
int g(int i)
{
  return 1 / i;
}

int main()
{
  while (1)
    {
      f(0);
      f(g(0));
    }
}
