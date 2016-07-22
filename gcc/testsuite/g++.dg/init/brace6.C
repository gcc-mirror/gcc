/* PR c++/30759 */
/* { dg-do compile } */

struct A {
   A(int) { }
};

struct B {
   B(const B&);
   int b;
};

struct C {};

struct D { int c; };

int main()
{
   int i = { 1 };
   int j = { 1, 2 }; /* { dg-error "8:scalar object 'j' requires one element" } */
   A a = { 6 }; /* { dg-error "6:in C\\+\\+98 'a' must be initialized" "" { target { ! c++11 } } } */
   B b = { 6 }; /* { dg-error "" } */
   C c = { 6 }; /* { dg-error "too many initializers" } */
   D d = { 6 };
}
