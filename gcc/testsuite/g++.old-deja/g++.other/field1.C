// { dg-do assemble  }

struct X
{
  static const bool b = true;
  static const int i = b ? 1 : 2;
};
