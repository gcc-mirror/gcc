// PR c++/31952

int
f0 (int bar)  // { dg-message "previously" }
try
{
  return 0;
}
catch (...)
{
  int bar = 0; // { dg-error "shadows a parameter" }
  return 1;
}

int
f1 (int bar)
try
{
  return 0;
}
catch (...)
{
  {
    int bar = 0; // Ok, not outermost block.
  }
  return 1;
}

int
f2 (int bar)
{
  try
    {
      return 0;
    }
  catch (...)
    {
      int bar = 0; // Ok, not a function-try-block.
      return 1;
    }
}
