// PR c++/101331
// { dg-do compile }
// { dg-options "-fsanitize-coverage=trace-pc -O2 -std=c++11" }

int a[2];
int b = 1;
int c { b && (a[b] = 0) };
