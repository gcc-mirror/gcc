// PR c++/43509
// { dg-do compile { target c++11 } }

typedef int B;			// { dg-message "" }
B::B() {}			// { dg-error "" }
