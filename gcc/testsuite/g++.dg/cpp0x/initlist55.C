// Test for -Wno-narrowing
// { dg-do compile { target c++11 } }
// { dg-options "-pedantic-errors -Wno-narrowing" }

int i;
float d = { i };
