// { dg-do assemble  }
// GROUPS passed operators
#include <iostream>

//
// ffrees space allocated for N-D array
//

template <class T>
void ffree(long rows, T** array)
{
for( long i = 0; i < rows; i++ )
  delete [] array[i];                   // delete row
delete [] array;                        // delete outer array
}

template <class T>
T* allocate1d(long size, T*& array)
{
return array = new T[size];
}

template <class T>
T** allocate2d(long d1, long d2, T**& array)
{
if( allocate1d(d1, array) != 0 )
  {
  for( long i = 0; i < d1; i++ )
    {
    if( allocate1d(d2, array[i]) == 0 )
      {
      ffree(i,array);
      return array;
      }
    }
  }
return array;
}

int main()
{
long d1 = 3, d2 = 4;
class foo
{
public:
foo() {std::cout << "foo created" << std::endl; }

~foo() {std::cout << "foo deleted" << std::endl; }
};

foo **f2;
allocate2d(d1, d2, f2);// { dg-error "" "" { target c++98 } }
ffree(d1, f2);// { dg-error "" "" { target c++98 } }

}
