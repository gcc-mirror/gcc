// PR c++/66210
// { dg-do compile { target c++14 } }

using resultType = const char*;

template<typename T>
T pi = (T)(3.1415926535897932385);

template<>
resultType pi<resultType> = "pi";

void foo()
{
  (void)pi<resultType>;
}
