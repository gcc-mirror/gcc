// PR c++/25999
// { dg-final { scan-assembler-not "_Z3Foov" } }

extern "C" {
  void Foo();
}
#pragma weak Random_Symbol
void Foo() { }

