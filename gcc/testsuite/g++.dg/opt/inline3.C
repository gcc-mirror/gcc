// PR opt/6793
// We failed to supress inlining of a varargs function when it's a template.
// { dg-do compile }
// { dg-options "-O3" }

#include <stdarg.h>

typedef __SIZE_TYPE__ size_t;

template < class Type > class VectorNd
{
  size_t size;
  Type *data;
 public:

  VectorNd (size_t _size, size_t count, ...)
	: size (_size)
  {
    data = new Type[size];

    va_list ap;

    va_start (ap, count);

    for (size_t i = 0; i < count; i++)
      data[i] = va_arg (ap, Type);

    va_end (ap);
  }

  ~VectorNd ()
  {
    delete [] data;
  }
};

int main ()
{
  VectorNd <double> vector (3, 3, 1.0, 2.0, 3.0);
}
