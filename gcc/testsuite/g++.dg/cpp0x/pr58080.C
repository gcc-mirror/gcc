// PR c++/58080
// { dg-do compile { target c++11 } }

template<class A, class B>
struct Eval
{
  void foo(A a, B b) { bar(a,b, 0); }
  auto bar(A a, B b, decltype(a+b)* _) -> decltype(a+b) { return a+b; }  // { dg-error "pointer" }
};

int main()
{
  Eval<int,void*> eiv; eiv.foo(0,0);
}
