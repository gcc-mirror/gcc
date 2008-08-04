// PR c++/37006
// { dg-options "-std=c++0x" }

template<class T>
struct A {
  template<class U>
  bool operator==(const A<U>&) = delete; // { dg-error "deleted function" }
  operator bool () { return true; }
};

int main()
{
  A<int> a1;
  A<void> a2;
  if(a1 == a2) {}		// { dg-error "used here" }
}
