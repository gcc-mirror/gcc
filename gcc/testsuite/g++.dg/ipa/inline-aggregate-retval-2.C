// A user-provided destructor makes the result an invisible reference.
// The comma expression requires a copy from the local into that result.
// Early inlining removes the empty destructor and its use of the local's
// address.  Keep the copy visible by limiting total scalarization.
// { dg-do compile }
// { dg-options "-O2 --param sra-max-scalarization-size-Ospeed=1 -fdump-ipa-fnsummary-details -fno-partial-inlining -fno-ipa-cp" }

struct V
{
  long d[8];
  ~V () {}
};

V
from_local (const V &a)
{
  V t = a;
  return (t.d[0] += 1, t);
}

// { dg-final { scan-ipa-dump {\*_[0-9]+\(D\) = t;\n[^\n]*\n\t\tWill be eliminated by inlining} "fnsummary" } }
