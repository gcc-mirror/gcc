// Test for -Wno-narrowing
// { dg-options "-std=c++0x -pedantic-errors -Wno-narrowing" }

int i;
float d = { i };
