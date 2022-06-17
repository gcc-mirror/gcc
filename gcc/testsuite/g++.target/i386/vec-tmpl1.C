// PR c++/65211
// { dg-additional-options "-Wno-psabi" }
// { dg-final { scan-assembler-not "movdqa" } }

typedef unsigned v4ui __attribute__ ((vector_size(16), aligned (16)));

template<int I>
static v4ui t1(unsigned *dest_data)
{
  typedef unsigned v4ui_1 __attribute__ ((vector_size (16), aligned (4)));
  return ((const v4ui_1*)dest_data)[0];
}

v4ui f(unsigned int *array)
{
    return t1<1>(array+7);
}
