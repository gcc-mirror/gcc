// { dg-do run }
// { dg-xfail-run-if "PR100409" { *-*-* } }

int __attribute__((const,noipa)) foo (int j)
{
  if (j != 0)
    throw 1;
  return 0;
}

int __attribute__((noipa)) bar (int *p, int n)
{
  int ret = 0;
  if (n)
    {
       foo (n);
       ret = *p;
    }
  ret += *p;
  return ret;
}

int main()
{
  try
    {
      return bar (nullptr, 1);
    }
  catch (...)
    {
      return 0;
    }
}
