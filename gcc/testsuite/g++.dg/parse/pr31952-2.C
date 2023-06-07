// PR c++/31952

// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.

void
f0()
{
  try
  {
  }
  catch (void *e)  // { dg-message "previously" }
  {
    void *e;       // { dg-error "redeclaration" }
  }
}

void
f1()
{
  try
  {
  }
  catch (void *e)
  {
    {
      void *e; // Ok, not outermost block.
    }
  }
}

void
f2()
try
{
}
catch (void *e)  // { dg-message "previously" }
{
  void *e;       // { dg-error "redeclaration" }
}

void
f3()
try
{
}
catch (void *e)
{
  {
    void *e; // Ok, not outermost block.
  }
}
