int
foo (a, b)
int *a,  *b;
{
  int x;
  *a = (*b + 1);
  x = *b;
  if ((int) x)
    return 1;
  else
    return 0;
}
