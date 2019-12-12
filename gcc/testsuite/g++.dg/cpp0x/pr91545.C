// PR c++/91545
// { dg-do compile { target c++11 } }

long a[1];
int d, e { d && (a[d] = 0) };
