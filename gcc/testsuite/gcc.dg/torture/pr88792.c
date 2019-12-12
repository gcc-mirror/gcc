/* { dg-do run } */

int one = 1;

char
__attribute__((noipa))
foo(char v)
{
  int modec;

  if (one)
    {
      modec = ((v < 'A' || v > 'Z') ? v : v + ('a' - 'A'));
      if (modec != 't' && modec != 'c' && modec != 'g')
	modec = 0;
    }
  else
    modec = 'g';

  return modec;
}

int main(int argc, char **argv)
{
  char c = 't';
  int r = foo (c);

  if (r != c)
    __builtin_abort ();

  return 0;
}
