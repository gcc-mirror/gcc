// { dg-options "-std=c++2b -fcontracts -fcontracts-nonattr -fcontracts-nonattr-const-keyword -fcontracts-nonattr-noconst " }

// this is mutable because we have const keywords and it is not marked const.
// fcontracts-nonattr-noconst is ignored.
void good (int x) pre (x++ > 1) {}

// this is const
// again, -fcontracts-nonattr-noconst is ignored
void bad (int x) pre const (x++ > 1) {} // { dg-error {increment of read-only location '\(const int\)x'} }
