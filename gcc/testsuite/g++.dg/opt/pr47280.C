// { dg-do compile }
// { dg-options "-O -fnon-call-exceptions -ftrapv" }

void bar (int n, char *p)
{
  try
    {
      n++;
      for (int i = 0; i < n - 1; i++)
	p[i];
    }
  catch (...)
    {}
}

