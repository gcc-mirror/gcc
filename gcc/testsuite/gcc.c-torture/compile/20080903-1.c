struct bar { unsigned short length; };

int
dummy(void)
{
  char c[4096];
  struct bar *a;
  struct bar *b;

  a->length = ((char *) b - c);
  return 0;
}
