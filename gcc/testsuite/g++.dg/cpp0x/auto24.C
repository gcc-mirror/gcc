// PR c++/48599
// { dg-do compile { target c++11 } }

int v[1];
auto (*p)[1] = &v;		// { dg-error "array of .auto" }
