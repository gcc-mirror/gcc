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
    catch (int)
    {
    }
  }
  catch (int)
  {
  }
}
