// PR c++/43509
// { dg-options "-std=c++0x" }

typedef int B;			// { dg-message "" }
B::B() {}			// { dg-error "" }
