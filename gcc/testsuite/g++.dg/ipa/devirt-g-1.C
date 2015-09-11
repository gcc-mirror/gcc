// { dg-do compile }
// { dg-options "-O2 -fdump-ipa-cp -fno-ipa-icf -fdump-tree-optimized" }

struct S { S(); virtual void xyzzy(); void otherstuff(); };
struct R { int a; S s; R(); };
S s;
R r;

void S::xyzzy ()
{
  otherstuff ();
  otherstuff ();
}

static void __attribute__ ((noinline)) foo(S *p) { p->xyzzy(); }
void bar() {foo(&s); }

static void __attribute__ ((noinline)) foh(S *p) { p->xyzzy(); }
void bah() {foh(&r.s); }

/* { dg-final { scan-ipa-dump "Discovered a virtual call to a known target.*S::xyzzy" "cp"  } } */
/* { dg-final { scan-tree-dump-times "OBJ_TYPE_REF" 0 "optimized"} } */
