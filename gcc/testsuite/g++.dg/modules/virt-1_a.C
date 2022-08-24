// { dg-module-do run }
// { dg-additional-options -fmodules-ts }
export module foo;
// { dg-module-cmi foo }

export struct Visitor
{
  virtual int Visit ();
};

// Key function explicitly not in line (regardless of p1779's state)
// We emit vtables & rtti only in this TU
int Visitor::Visit ()
{
  return 0;
}

export int Visit (Visitor *v)
{
  return v->Visit ();
}

// { dg-final { scan-assembler {_ZTVW3foo7Visitor:} } }
// { dg-final { scan-assembler {_ZTIW3foo7Visitor:} } }
// { dg-final { scan-assembler {_ZTSW3foo7Visitor:} } }
// { dg-final { scan-assembler {_ZW3foo5VisitPS_7Visitor:} } }
