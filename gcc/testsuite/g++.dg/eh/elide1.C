// PR c++/13944

// Bug: When eliding the copy from the A temporary into the exception
// object, we extended the throw prohibition to the constructor for the
// temporary.  This is wrong; the throw from A() should propagate normally
// regardless of the elision of the temporary.

// { dg-do run }

struct A
{
  A() { throw 0; }
};

int main()
{
  try
    {
      throw A();
    }
  catch(int i)
    {
      return i;
    }
  catch (...)
    {
      return 2;
    }
  return 3;
}
