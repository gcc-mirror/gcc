// PR c++/31952

// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.

int
f0 (int bar)     // { dg-message "previously" }
try
{
  return 0;
}
catch (int bar)  // { dg-error "shadows a parameter" }
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
