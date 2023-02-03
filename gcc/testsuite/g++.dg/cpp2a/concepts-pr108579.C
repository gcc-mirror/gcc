// PR c++/108579
// { dg-do compile { target c++20 } }

template<class T>
struct A {
  A(double, char);
  A(int) requires requires { A(0.0, 'c'); };
  A& operator=(int) requires requires { A(1.0, 'd'); };
};

int main() {
  A<int> a(3);
  a = 5;
}
