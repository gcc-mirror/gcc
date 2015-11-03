// PR c++/56134
// { dg-require-alias "" }
// { dg-options "-g" }

char a;
class Q
{
  static char q __attribute__ ((alias ("a")));
};
