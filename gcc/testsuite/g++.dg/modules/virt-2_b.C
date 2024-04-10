// { dg-additional-options -fmodules-ts }

import foo;

struct Mine : Visitor 
{
  int Visit () override
  {
    return 1;
  }
};

extern int Foo ();

int main ()
{
  Mine me;

  if (auto b = Foo ())
    return b;
  return !(Visit (&me) == 1);
}

// Again, we emit Visitor vtable and rtti here
// { dg-final { scan-assembler {_ZTVW3foo7Visitor:} } }
// { dg-final { scan-assembler {_ZTIW3foo7Visitor:} } }
// { dg-final { scan-assembler {_ZTSW3foo7Visitor:} } }
