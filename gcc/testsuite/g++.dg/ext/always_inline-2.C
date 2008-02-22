// { dg-options "-O0" }
// { dg-do compile }
// PR C++/34715


namespace X
{
 template <class T>
 const T& min123(const T& a, const T& b);
}


template <class T>
inline __attribute__ ((always_inline)) const T& X::min123(const T& a, const T& b)
{
 return a < b ? a : b;
}
int main()
{
 int a, b;
 return X::min123(a, b);
}



// { dg-final { scan-assembler-not "min123" } }
