// Bug: g++ doesn't allow const objects to be constructed.
// Build don't link:

struct B { B(); };

const B foo()
{
  return B();			// gets bogus error - constructing const
}
