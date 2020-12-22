// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }

export module frob;
// { dg-module-cmi "frob" }

export template <typename T>
class X
{
  T m;

public:
  void frob (T v) { m = v; }

  T frobber (T v) { return v + m; }
};
