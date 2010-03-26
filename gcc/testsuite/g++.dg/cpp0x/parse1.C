// PR c++/43509
// { dg-options "-std=c++0x" }

typedef int B;			// { dg-error "" }
B::B() {}			// { dg-error "" }
