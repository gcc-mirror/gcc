// AAPCS overrides TARGET_CXX_KEY_METHOD_MAY_BE_INLINE,
// in a way that invalidates this test.
// { dg-skip-if "!TARGET_CXX_KEY_METHOD_MAY_BE_INLINE" { arm_eabi } } 
// { dg-module-do run }
// { dg-additional-options -fmodules-ts }
export module foo;
// { dg-module-cmi foo }

export struct Visitor
{
  virtual int Visit ();
};

// Key function explicitly inline (regardless of p1779's state)
// We emit vtables & rtti only in this TU
inline // Yoink!
  int Visitor::Visit ()
{
  return 0;
}

export int Visit (Visitor *v)
{
  return v->Visit ();
}

// Emit here
// { dg-final { scan-assembler {_ZTVW3foo7Visitor:} } }
// { dg-final { scan-assembler {_ZTIW3foo7Visitor:} } }
// { dg-final { scan-assembler {_ZTSW3foo7Visitor:} } }
