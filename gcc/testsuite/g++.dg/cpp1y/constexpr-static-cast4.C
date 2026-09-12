// { dg-do compile { target c++14 } }

struct A {};
struct B : A {};
struct C { int c; };
struct D { int d; };
struct E : C, D { int e; };

constexpr bool
foo (bool x)
{
  struct A a = {};
  A *b = &a;
  A *c = nullptr;
  if (x)
    {
      c = static_cast <A *> (static_cast <B *> (static_cast <A *> (static_cast <B *> (b))));	// { dg-error "'A' operand is not a base class subobject of a 'B' object" }
    }
  return true;
}

constexpr bool a = foo (true);

constexpr bool
bar (bool x)
{
  struct D a = { 42 };
  D *b = &a;
  D *c = nullptr;
  if (x)
    {
      c = static_cast <D *> (static_cast <E *> (static_cast <D *> (static_cast <E *> (b))));	// { dg-error "'D' operand is not a base class subobject of a 'E' object" }
    }
  return true;
}

constexpr bool b = bar (true);
