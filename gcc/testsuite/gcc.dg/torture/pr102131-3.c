/* { dg-do run } */

int a;
int main()
{
  unsigned b = 0;
  for (a = 2; a < 8; a += 2)
    if (++b > a)
      __builtin_abort();
  return 0;
}
