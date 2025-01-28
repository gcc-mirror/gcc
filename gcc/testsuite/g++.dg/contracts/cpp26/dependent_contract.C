// check that dependent contract check does not cause an ICE
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -fcontract-evaluation-semantic=observe " }
template <typename ST>
struct S{

  int* check() const
  {
      static int i = 6;
      return &i;
  }

  template <typename T>
  T* tcheck(T) const{
    static T t;
    return nullptr;
  }

  void f() pre(check()) pre(tcheck(1)){
  }

  template <typename T>
  void f(T t) pre(check()) pre(tcheck(1)) pre(tcheck<T>(t)){
  }

};

int main() {
    S<int> s;
    s.f();
    s.f(1);
}
