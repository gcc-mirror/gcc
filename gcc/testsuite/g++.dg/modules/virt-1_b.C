// { dg-additional-options -fmodules-ts }

import foo;

struct Mine : Visitor 
{
  int Visit () override
  {
    return 1;
  }
};

int main ()
{
  Mine me;

  return !(Visit (&me) == 1);
}

// We do not emit Visitor vtable or rtti here
// { dg-final { scan-assembler-not {_ZTV7Visitor:} } }
// { dg-final { scan-assembler-not {_ZTI7Visitor:} } }
// { dg-final { scan-assembler-not {_ZTS7Visitor:} } }
