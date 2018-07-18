// Testcase from P0136
// { dg-do compile { target c++11 } }
// { dg-options "-fnew-inheriting-ctors -fdump-tree-gimple -O2 -fno-inline" }

struct W { W(int); };
struct V: W { using W::W; };
struct X : virtual V { using V::V; X() = delete; };
struct Y : X { using X::X; };
struct Z : Y, virtual V { using Y::Y; };
Z z(0); // OK: initialization of Y does not invoke default constructor of X

// Check that we're passing this and __vtt along to the Y inheriting
// constructor, but not the int parameter.
// { dg-final { scan-assembler "_ZN1YCI21WEi" } }
// { dg-final { scan-tree-dump "Y::Y ._2, _3.;" "gimple" } }

// And that we aren't expecting the int, either.
// { dg-final { scan-tree-dump-not "Y::Y.int\[^\n\]*int" "gimple" } }

// And that we *are* passing the int along to V::V.
// { dg-final { scan-assembler "_ZN1VCI21WEi" } }
// { dg-final { scan-tree-dump "V::V .this, _1.;" "gimple" } }
