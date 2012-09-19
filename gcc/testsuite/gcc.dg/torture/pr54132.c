/* { dg-do run } */

extern void abort (void);
void foo(char *p, int n)
{
  int i;
  for (i = 1; i < n; i++)
    p[i] = p[i - 1];
}
int main()
{
  char a[1024];
  a[0] = 1;
  foo (a, 1024);
  if (a[1023] != 1)
    abort ();
  return 0;
}
