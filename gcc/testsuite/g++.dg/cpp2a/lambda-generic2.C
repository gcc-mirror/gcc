// P0428R2
// { dg-do compile }

int j = []<class T>(T t, int i) { return i; }(3, 4);
// { dg-error "lambda templates are only available with" "" { target c++17_down } .-1 }
// { dg-error "lambda expressions only available with" "" { target c++98_only } .-2 }
// { dg-error "invalid use of 'auto'" "" { target c++98_only } .-3 }
