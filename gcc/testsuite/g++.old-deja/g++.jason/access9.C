// { dg-do assemble  }
// Bug: g++ doesn't allow const objects to be constructed.

struct B { B(); };

const B foo()
{
  return B();			// { dg-bogus "" } constructing const
}
