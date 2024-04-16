// { dg-additional-options -fmodules-ts }

import foo;

int Foo ()
{
  Visitor v;

  return !(Visit (&v) == 0);
}

// Since https://github.com/itanium-cxx-abi/cxx-abi/pull/171
// we only emit Visitor vtables and RTTI in its module unit
// { dg-final { scan-assembler-not {_ZTVW3foo7Visitor:} } }
// { dg-final { scan-assembler-not {_ZTIW3foo7Visitor:} } }
// { dg-final { scan-assembler-not {_ZTSW3foo7Visitor:} } }
