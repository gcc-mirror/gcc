// { dg-do compile }

int i[4] = { { 3 } }; // { dg-error "braces" "" { target c++98_only } }
int j[4] = { { { 3 } } }; // { dg-error "braces" }

