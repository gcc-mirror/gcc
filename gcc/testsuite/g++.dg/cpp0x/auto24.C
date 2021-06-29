// PR c++/48599
// { dg-do compile { target c++11 } }
// Allowed since DR2397.

int v[1];
auto (*p)[1] = &v;
