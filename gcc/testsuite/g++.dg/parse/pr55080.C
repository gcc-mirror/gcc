// PR c++/55080
// { dg-options "-std=c++98 -pedantic" }

class B {
 static const int c = 3.1415926; // { dg-warning "constant-expression" }
};
