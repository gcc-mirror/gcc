// { dg-module-do link }
// { dg-additional-options -fmodules-ts }

export module foo:exp;
// { dg-module-cmi foo:exp }

export class Foo
{
  Foo ();

public:
  void Func ();

  static Foo *Factory ();
};
