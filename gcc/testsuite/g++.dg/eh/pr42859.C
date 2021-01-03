// { dg-do compile }

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
