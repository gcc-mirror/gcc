// { dg-options "-std=c++23 -fcontracts -fcontracts-nonattr -fcontracts-nonattr-noconst" }

// This should compile without error.

class X {

static int calc ()
  post (alignment: 0 == (alignment & (alignment-1)));

};

