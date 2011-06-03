// PR c++/45418
// { dg-options -std=c++0x }

struct A1 { };
struct A2 {
  A2();
};

template <class T> struct B {
  T ar[1];
  B(T t):ar({t}) {}
};

int main(){
  B<int> bi{1};
  A1 a1;
  B<A1> ba1{a1};
  A2 a2;
  A2 a2r[1]{{a2}};
  B<A2> ba2{a2};
}
