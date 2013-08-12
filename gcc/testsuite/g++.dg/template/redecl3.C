// PR c++/19980
// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>

// { dg-do compile }

int foo;                    // { dg-message "previous declaration" }
template<int> void foo() {} // { dg-error "redeclared" }
