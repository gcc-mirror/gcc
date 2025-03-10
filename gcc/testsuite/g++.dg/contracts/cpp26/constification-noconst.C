// { dg-options "-std=c++2b -fcontracts -fcontracts-nonattr -fcontracts-nonattr-noconst" }

void good (int x) pre (x++ > 1) {} // should not be an error.
