// { dg-options "-std=c++1z -fconcepts" }

template<typename ... T>
  concept bool C1 = true;

template<int ... N>
  concept bool C2 = true;

C1{...A} void f1() {};
C2{...A} void f2() {};

int main()
{
  f1<int, short, char>();
  f2<1, 2, 3>();
  return 0;
}
