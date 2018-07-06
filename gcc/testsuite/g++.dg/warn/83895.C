// { dg-additional-options -Wparentheses }
// { dg-do compile { target c++11 } }

struct X;

typedef int (X::*foo);
using bar = int (X::*);
