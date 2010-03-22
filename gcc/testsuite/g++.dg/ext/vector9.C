// PR c++/34891

typedef float v4f __attribute__((vector_size(8)));
typedef int   v4i __attribute__((vector_size(8)));

void foo()
{
  v4f v;
  !(v4i)v; // { dg-error "__vector.2. int|argument" }
}
