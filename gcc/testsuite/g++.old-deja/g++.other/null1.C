// Build don't run:

#include <cstddef>

void g(int) {}
void g(long) {}
extern void g(void*);

template <int I>
void h() {}

void k(int) {}

template <class T>
void l(T);

template <>
void l(int) {}

int main()
{
  int i = NULL; // WARNING - converting NULL to non-pointer type
  float z = NULL; // WARNING - converting NULL to non-pointer type
  int a[2];

  i != NULL; // WARNING - NULL used in arithmetic
  NULL != z; // WARNING - NULL used in arithmetic
  k != NULL; // No warning: decay conversion
  NULL != a; // Likewise.
  -NULL;     // WARNING - converting NULL to non-pointer type
  +NULL;     // WARNING - converting NULL to non-pointer type
  ~NULL;     // WARNING - converting NULL to non-pointer type
  a[NULL] = 3; // WARNING - converting NULL to non-pointer-type
  i = NULL;  // WARNING - converting NULL to non-pointer type
  z = NULL;  // WARNING - converting NULL to non-pointer type
  k(NULL);   // WARNING - converting NULL to int
  g(NULL);   // WARNING - converting NULL to int
  h<NULL>(); // WARNING - NULL bound to integer template parameter
  l(NULL);   // WARNING - converting NULL to int
  NULL && NULL; // No warning: converting NULL to bool is OK
}
