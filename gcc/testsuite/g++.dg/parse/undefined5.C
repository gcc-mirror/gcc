// PR c++/5975
// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>
// { dg-do compile }

typedef typename X::Y<> y;  // { dg-error "" }
