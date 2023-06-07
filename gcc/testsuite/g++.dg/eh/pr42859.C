// { dg-do compile }
// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.

void start (void);
void
ptw32_terminate (void)
{
  try
  {
    try
    {
      start ();
    }
    catch (int)
    {
    }
    catch (int) // { dg-warning "will be caught by earlier handler" }
    {
    }
  }
  catch (int)
  {
  }
}
