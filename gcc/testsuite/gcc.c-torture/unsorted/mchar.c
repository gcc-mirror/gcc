int
foo (char *a, char *b)
{
  int x;
  *a = *b;
  x = *b;
  if ((char) x)
    return 1;
  else
    return 0;
}
