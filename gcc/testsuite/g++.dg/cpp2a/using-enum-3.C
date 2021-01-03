// Test of 'using enum' syntax error recovery.
// { dg-do compile { target c++20 } }

using enum 2 + garbage3'850%^&;		// { dg-error "" }

void f() {}
