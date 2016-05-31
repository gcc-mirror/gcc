// { dg-do compile }

extern int j;

void
f (int i)
{
  switch (i) // { dg-warning "statement will never be executed" }
    {
      try
      {
      }
      catch (...)
      {
      }
    case 1:;
    }
}

void
g (int i)
{
  switch (i)
    {
      try
      {
	j = 42;  // { dg-warning "statement will never be executed" }
      }
      catch (...)
      {
      }
    case 1:;
    }
}
