// PR c++/31952

int
f0 (int bar)     // { dg-message "previously" }
try
{
  return 0;
}
catch (int bar)  // { dg-error "redeclaration" }
{
  return 1;
}

int
f1 (int bar)
{
  try
  {
    return 0;
  }
  catch (int bar)  // Ok, not a function-try-block.
  {
    return 1;
  }
}
