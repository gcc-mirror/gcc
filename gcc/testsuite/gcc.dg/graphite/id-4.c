extern int a[];
void
g ()
{
  int i, b;
  for (i = 0; i < 10; i++)
    a[i] = (b == 0);
}
