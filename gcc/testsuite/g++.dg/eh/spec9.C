// PR c++/15745
// { dg-do run }

typedef int IntArray[10];
IntArray i;

void test_array()
#if __cplusplus <= 201402L
throw (IntArray)	// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } }
#endif
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
