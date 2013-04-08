// PR c++/17232 (DR 337)

template<typename T>
class A {
  virtual void f() = 0;
};

template<typename T>
void g(T (*a)[1]) {}		// { dg-error "abstract" "" { xfail *-*-* } }

int main() {
  g<A<int> >(0);  // { dg-error "no matching function" "" { xfail *-*-* } }
}
