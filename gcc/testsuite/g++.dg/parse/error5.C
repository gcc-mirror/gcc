// PR c++/13269

class Foo { int foo() return 0; } }; // { dg-error "" }

