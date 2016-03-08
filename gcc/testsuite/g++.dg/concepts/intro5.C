// { dg-options "-std=c++1z -fconcepts" }

template<typename T, typename U = int>
  concept bool C()
  {
     return sizeof(U) == sizeof(int);
  }

C{A} void f1() {}

int main()
{
  f1<char>();
  return 0;
}
