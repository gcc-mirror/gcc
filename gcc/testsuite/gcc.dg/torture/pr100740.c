/* { dg-do run } */

unsigned a, b;
int main()
{
  unsigned c = 0;
  for (a = 0; a < 2; a++)
    for (b = 0; b < 2; b++)
      if (++c < a)
	__builtin_abort ();
  return 0;
}
