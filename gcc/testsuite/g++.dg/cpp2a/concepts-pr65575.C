// PR c++/65575
// { dg-do compile { target c++17 } }
// { dg-additional-options "-fconcepts" }

template<typename T>
concept C = false;

int f1() requires false;  // { dg-error "constraints on a non-templated function" }
int& f2() requires false; // { dg-error "constraints on a non-templated function" }
int* f3() requires false; // { dg-error "constraints on a non-templated function" }
auto f4() -> int& requires false; // { dg-error "constraints on a non-templated function" }
auto f5() -> int* requires false; // { dg-error "constraints on a non-templated function" }
auto f6() -> int requires false;  // { dg-error "constraints on a non-templated function" }

int (*p1)() requires true; // { dg-error "" }
int (&p2)() requires true; // { dg-error "" }
int g(int (*)() requires true); // { dg-error "" }

int
main()
{
  f1();
}
