struct One { };
struct Two { };

void
handle_unexpected ()
{
  try
  {
    throw;
  }
  catch (One &)
  {
    throw Two ();
  }
}

void
doit () throw (Two)
{
  throw One ();
}
