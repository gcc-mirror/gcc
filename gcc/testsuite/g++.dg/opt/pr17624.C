// { dg-do compile }
// { dg-options "-O2" }

extern void foo (void);
int c;
void foo (int n)
{
  int j = 0;
  try
    {
      for(;;)
        {
          foo ();
          if (j ++ == n)
            break;
          foo ();
        }
    }
  catch (...)
    {
      c = j;
    }
}
