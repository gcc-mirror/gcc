// { dg-do compile }

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
  catch (int)
    {
    }
}

