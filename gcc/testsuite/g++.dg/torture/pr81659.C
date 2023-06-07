// { dg-do compile }
// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.

void
a (int b)
{
  if (b)
    throw;
  try
    {
      a (3);
    }
  catch (int)
    {
    }
  catch (int) // { dg-warning "will be caught by earlier handler" }
    {
    }
}

