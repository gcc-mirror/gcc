// PR c++/70035
// { dg-do run }
// { dg-shouldfail "ubsan" }
// { dg-options "-fsanitize=vptr -fno-sanitize-recover=undefined -fno-implicit-constexpr" }

struct A {
  A (int) {}
  virtual int foo () { return 1; }
};
struct B : public A {
  using A::foo;
  B (int x) : A (foo (x)) {}
  int foo (int x) { return x * 2; }
};

int
main ()
{
  B b (20);
}

// { dg-output "\[^\n\r]*pr70035.C:12:\[0-9]*: runtime error: member call on address 0x\[0-9a-fA-F]* which does not point to an object of type 'B'(\n|\r\n|\r)" }
// { dg-output "0x\[0-9a-fA-F]*: note: object has invalid vptr(\n|\r\n|\r)" }
// { dg-output "  ?.. .. .. ..  ?.. .. .. ..  ?.. .. .. .. \[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "              ?\\^~~~~~~~~~~\[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "              ?invalid vptr" }
