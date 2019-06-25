// PR c++/90969
// { dg-do compile }

__attribute__ ((__vector_size__ (4))) int v;
int &a = v[0];
