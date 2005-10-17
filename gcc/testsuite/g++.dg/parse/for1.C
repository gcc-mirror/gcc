// PR c++/23440
// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>
// { dg-do compile }
// { dg-options "" }

# 0 "for1.C"
void foo() { for (;;)  // { dg-error "at end of input" "" { target *-*-* } 0 }
