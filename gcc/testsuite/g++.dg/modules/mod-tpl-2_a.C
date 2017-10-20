// { dg-module-do run }

export module frob;
// { dg-module-bmi "frob" }

export template <typename T>
class X
{
  T m;

public:
  void frob (T v) { m = v; }

  T frobber (T v) { return v + m; }
};
