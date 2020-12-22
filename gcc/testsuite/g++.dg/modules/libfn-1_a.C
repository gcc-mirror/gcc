// { dg-additional-options -fmodules-ts }
// Make sure we're not confused by an imported declaration of a
// library fn
export module foo;
// { dg-module-cmi foo }

export inline void thrower ()
{
  try 
    {
      throw 1;
    }
  catch (...)
    {
    }
}
