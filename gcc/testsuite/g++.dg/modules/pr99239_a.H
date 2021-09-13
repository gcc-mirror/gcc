// PR 99239 ICE on catch clause
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

inline void Foo ()
{
  try
    {}
  catch(...)
    {
    }
}

