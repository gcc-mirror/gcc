// { dg-do compile }
// { dg-options "-std=c++11" }

struct S {
  long long l;
} s {(long long) &s};
