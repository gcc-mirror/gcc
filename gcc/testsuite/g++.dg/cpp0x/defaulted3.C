// PR c++/37006
// { dg-do compile { target c++11 } }

template<class T>
struct A {
  template<class U>
  bool operator==(const A<U>&) const = delete; // { dg-message "declared" }
  operator bool () { return true; }
};

int main()
{
  A<int> a1;
  A<void> a2;
  if(a1 == a2) {}		// { dg-error "use" }
}
