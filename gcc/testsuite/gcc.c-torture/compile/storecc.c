int
foo (char *p, int a)
{
  *p = a;
  if ((char) a)
    return 1;
}
