// { dg-options "-O0" }
// { dg-do compile }
// PR C++/34715


namespace X
{
 template <class T>
 const T& min(const T& a, const T& b);

 template <class T>
 inline __attribute__ ((always_inline)) const T& min(const T& a, const T& b)
 {
  return a < b ? a : b;
 }
}
template <class T>
inline __attribute__ ((always_inline)) T y(const T& a, const T& b)
{
 return X::min(a, b);
}
int main()
{
 int a = 0, b = 0;
 return y(a, b);
}
