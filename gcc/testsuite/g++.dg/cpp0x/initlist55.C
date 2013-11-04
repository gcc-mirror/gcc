// Test for -Wno-narrowing
// { dg-options "-std=c++11 -pedantic-errors -Wno-narrowing" }

int i;
float d = { i };
