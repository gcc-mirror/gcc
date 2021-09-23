// PR c++/97406
// { dg-do compile { target c++20 } }

struct X {
  void f() { }
  int a;
  int arr[5];
};

// Duplicated so that I can check dg-message.
template<typename T>
requires (sizeof(T)==1) // { dg-message {\[with T = void \(X::\*\)\(\)\]} }
void f1(T)
{ }

template<typename T>
requires (sizeof(T)==1) // { dg-message {\[with T = int X::\*\]} }
void f2(T)
{ }

template<typename T>
requires (sizeof(T)==1) // { dg-message {\[with T = int \(X::\*\)\[5\]\]} }
void f3(T)
{ }

int main()
{
  f1(&X::f); // { dg-error "no matching function for call" }
  f2(&X::a); // { dg-error "no matching function for call" }
  f3(&X::arr); // { dg-error "no matching function for call" }
}
