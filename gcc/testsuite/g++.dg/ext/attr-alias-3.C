// PR c++/56134
// { dg-require-alias "" }

char a;
class Q
{
  static char q __attribute__ ((alias ("a")));
};
