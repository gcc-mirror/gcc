// PR c++/14401

struct { struct { int& i ; } bar ; } foo ; // { dg-error "" }
