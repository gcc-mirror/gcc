// { dg-do assemble  }
// GROUPS passed visibility
// Make sure private inheritance affects the visibility of
// static members used in an inherited context.
class foo
{
public:
  static int y; // { dg-error "" } private
};
class foo1 : private foo
{ };
class foo2 : public foo1
{ public:
  void bar () { y; }// { dg-error "" } .*
};
