void f(int n)
{
bugcauser:
  if (n != 0)
    f(n-1);
  return;
}
