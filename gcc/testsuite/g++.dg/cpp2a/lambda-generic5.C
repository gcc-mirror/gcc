// P0428R2
// { dg-do compile }
// { dg-options "-std=c++2a" }

int j = []<class T>(T t, int i) { return i; }(3, 4);
// { dg-bogus "lambda templates are only available with" "" { target c++20 } .-1 }
