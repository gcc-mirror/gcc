// PR c++/114983
// { dg-do compile { target c++11 } }
// { dg-options "-Wsizeof-array-div" }

using size_t = decltype (sizeof (0));
unsigned int samplesBuffer[40];

template <typename T>
constexpr inline size_t fn1()
{
  return ((sizeof(samplesBuffer)) / (sizeof(T))); // { dg-bogus "expression does not compute" }
}

template <typename T>
constexpr inline size_t fn2()
{
  return ((sizeof(samplesBuffer)) / sizeof(T)); // { dg-warning "expression does not compute" }
}

size_t
g ()
{
  auto sz = sizeof (samplesBuffer) / (sizeof(unsigned char));
  sz += fn1<unsigned char>();
  sz += fn2<unsigned char>(); // { dg-message "required from here" }
  return sz;
}
