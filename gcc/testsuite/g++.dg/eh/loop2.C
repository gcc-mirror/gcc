// Test that breaking out of a handler works.
// { dg-do run }

int main ()
{
  while (1)
    {
      try { throw 1; }
      catch (...) { break; }
    }
}
