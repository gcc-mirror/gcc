/* { dg-do run } */

int a;
int main()
{
  unsigned b = 0;
  int c = 1;
  for (; b < 3; b++)
    {
      while (c < b)
	__builtin_abort ();
      for (a = 0; a < 3; a++)
	c++;
    }
  return 0;
}
