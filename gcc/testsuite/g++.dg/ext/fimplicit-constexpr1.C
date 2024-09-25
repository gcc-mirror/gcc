// { dg-additional-options -fimplicit-constexpr }
// { dg-do compile { target c++14 } }

void f() { } // { dg-message "'-fimplicit-constexpr' only affects 'inline' functions" }

inline int g() { f(); return 42; } // { dg-error {non-'constexpr' function 'void f\(\)'} }

constexpr int i = g(); // { dg-error {'int g\(\)' called in a constant expression} }
