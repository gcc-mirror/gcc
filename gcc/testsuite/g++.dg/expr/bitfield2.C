// PR c++/27979

class Ast
{
  enum AstKind { };
  const AstKind kind : 8;
  void foo(AstKind k) { }
  void bar(void) { foo(kind); }
};
