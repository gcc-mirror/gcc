// { dg-options "-std=c++17 -fconcepts" }

template<typename T>
concept bool C = false;

int f1() requires false;
int& f2() requires false;
int* f3() requires false;
auto f4() -> int& requires false;
auto f5() -> int* requires false;
auto f6() -> int requires false;

int (*p)() requires true; // { dg-error "" }
int (&p)() requires true; // { dg-error "" }
int g(int (*)() requires true); // { dg-error "" }

int f() { }

int
main()
{
  f1(); // { dg-error "cannot call" }
}
