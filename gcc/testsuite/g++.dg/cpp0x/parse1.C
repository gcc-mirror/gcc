// PR c++/43509
// { dg-options "-std=c++11" }

typedef int B;			// { dg-message "" }
B::B() {}			// { dg-error "" }
