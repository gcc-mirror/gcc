// PR c++/90107
// { dg-do compile }

struct A;
namespace N { extern A a; }
struct A {} ::N::a;

struct A1;
struct B { static A1 a1; };
struct A1 {} ::B::a1;
