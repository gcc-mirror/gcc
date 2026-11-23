// A by-value 'operator*' copies its local result into the return value.
// The copy is eliminated by inlining, so it does not count towards the early
// inliner growth and the operator fits in --param early-inlining-insns.
// { dg-do compile }
// { dg-options "-O3 --param early-inlining-insns=16 -fno-inline-functions -fno-partial-inlining -fdump-tree-einline-optimized" }

struct V {
  long d[8];
  V &operator*= (const V &o) { for (int i = 0; i < 8; i++) d[i] *= o.d[i]; return *this; }
};

static V operator* (const V &a, const V &b) { V t = a; return t *= b; }

V ga, gb, gc, r;

void use () { r = ga * gb * gc; }

// { dg-final { scan-tree-dump-times {Inlining V operator\*\(const V&, const V&\)/[0-9]+ into void use\(\)/[0-9]+} 2 "einline" } }
