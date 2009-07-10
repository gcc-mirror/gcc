// PR c++/37877
// { dg-do compile }

extern "C++" struct S
{
  static int x;
} s;
