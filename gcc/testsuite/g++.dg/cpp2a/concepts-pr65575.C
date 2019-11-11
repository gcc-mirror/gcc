// PR c++/65575
// { dg-do compile { target c++17_only } }
// { dg-additional-options "-fconcepts" }

template<typename T>
concept bool C = false;

int f1() requires false;
int& f2() requires false;
int* f3() requires false;
auto f4() -> int& requires false;
auto f5() -> int* requires false;
auto f6() -> int requires false;

int (*p1)() requires true; // { dg-error "" }
int (&p2)() requires true; // { dg-error "" }
int g(int (*)() requires true); // { dg-error "" }

int
main()
{
  f1(); // { dg-error "" }
}
