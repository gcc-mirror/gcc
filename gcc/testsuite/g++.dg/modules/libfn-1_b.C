// { dg-additional-options -fmodules-ts }
import foo;

void bar ()
{
  thrower ();
}

void baz ()
{
  try 
    {
      throw 1;
    }
  catch (...)
    {
    }
}
