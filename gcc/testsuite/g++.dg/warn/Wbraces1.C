// PR c++/19755
// { dg-options "-Wmissing-braces" }
int a[2][2] = { 0, 1 , 2, 3 }; // { dg-warning "" }
