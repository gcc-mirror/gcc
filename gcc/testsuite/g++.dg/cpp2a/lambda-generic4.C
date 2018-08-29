// P0428R2
// { dg-do compile }
// { dg-options "-Wno-pedantic" }

int j = []<class T>(T t, int i) { return i; }(3, 4);
// { dg-warning "lambda templates are only available with" "" { target c++11_down } .-1 }
// { dg-warning "lambda expressions only available with" "" { target c++98_only } .-2 }
// { dg-error "invalid use of 'auto'" "" { target c++98_only } .-3 }
