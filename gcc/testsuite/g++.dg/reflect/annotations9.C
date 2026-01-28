// PR c++/123837
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// { dg-additional-options "-flto" { target lto } }

struct A {};
[[=A {}]] int a {};
struct [[=A {}]] B { int b; };
B b {};

int
main ()
{
}
