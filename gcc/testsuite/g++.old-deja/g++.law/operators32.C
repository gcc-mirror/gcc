// Build don't link: 
// GROUPS passed operators
#include <iostream.h>

//
// frees space allocated for N-D array
//

template <class T>
void free(long rows, T** array)
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
      free(i,array);
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
foo() {cout << "foo created" << endl; }

~foo() {cout << "foo deleted" << endl; }
};

foo **f2;
allocate2d(d1, d2, f2);// ERROR -  type.*// ERROR -    trying to.*
free(d1, f2);// ERROR -  type.*// ERROR -    trying to.*

}
