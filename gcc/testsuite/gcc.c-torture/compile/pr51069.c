
int a, b, c, d, e, f, bar (void);

void
foo (int x)
{
  for (;;)
    {
      if (!x)
        {
          for (d = 6; d >= 0; d--)
            {
              while (!b)
                ;
              if (e)
                return foo (x);
              if (f)
                {
                  a = 0;
                  continue;
                }
              for (; c; c--)
                ;
            }
        }
      if (bar ())
        break;
      e = 0;
      if (x)
        for (;;)
          ;
    }
}


