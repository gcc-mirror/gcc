// PR c++/15745
// { dg-do run }

typedef int IntArray[10];
IntArray i;

void test_array() throw (IntArray)
{
  throw i;
}

int main ()
{
  try
    {
      test_array();
    }
  catch (IntArray) {}
}
