// PR c++/44522
// { dg-do compile }

namespace x {
  struct a { };
  a A0;
}

x:a a2;				// { dg-error "nested-name-specifier" }
x::a a3 = a2;

x:a f (void)			// { dg-error "nested-name-specifier" }
{
  x::a a4;			// x:a would parse like a label
  return a4;
}

x::a g (x:a a4)			// { dg-error "nested-name-specifier" }
{
  return a4;
}

class B
{
  x::a f(void)			// x:a would parse like a bitfield
  {
    x::a a4;
    a4 = x:A0;			// { dg-error "nested-name-specifier" }
    return a4;
  }
};
