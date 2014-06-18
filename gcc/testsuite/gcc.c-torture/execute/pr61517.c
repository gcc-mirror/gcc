int a, b, *c = &a;
unsigned short d;

int
main ()
{
  unsigned int e = a;
  *c = 1;
  if (!b)
    {
      d = e;
      *c = d | e;
    }

  if (a != 0)
    __builtin_abort ();

  return 0;
}
