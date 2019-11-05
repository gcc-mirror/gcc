// This should continue to work.
// { dg-do compile { target c++2a } }

template<class T>
struct A {
  template<class U>
  bool operator==(const A<U>&);
};

int main()
{
  A<int> a1;
  A<void> a2;
  return a1 == a2;
}
