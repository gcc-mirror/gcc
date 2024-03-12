// { dg-do compile { target c++11 } }
// { dg-options "" }
// Empty dg-options to override -pedantic-errors.

typedef long CFIndex;
typedef enum CFComparisonResult : CFIndex CFComparisonResult;
// { dg-warning "declaration of enumeration with fixed underlying type" "" { target *-*-* } .-1 }
