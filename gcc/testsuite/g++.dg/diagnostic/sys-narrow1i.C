// { dg-do compile { target c++11 } }
// { dg-options "-fpermissive -w" }

// No diagnostic
int i = { 2.4 }; // C++11 error: narrowing conversion

