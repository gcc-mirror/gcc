void abort (void);
void exit (int);

char *
f (char *s, unsigned int i)
{
  return &s[i + 3 - 1];
}

int
main (void)
{
  char *str = "abcdefghijkl";
  char *x2 = f (str, 12);
  if (str + 14 != x2)
    abort ();
  exit (0);
}
