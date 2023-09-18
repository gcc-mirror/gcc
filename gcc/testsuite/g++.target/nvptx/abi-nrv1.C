// { dg-do compile }
// { dg-additional-options "-m64" }

// Check NRV optimization doesn't change the PTX prototypes.

struct A
{
  int d;

// { dg-final { scan-assembler-times ".weak .func _ZN1AC1Ev \\(.param.u64 %in_ar0\\)(?:;|\[\r\n\]+\{)" 2 } }
  A () { d = 123; }
  A (const A & o) { d = o.d; }
  void frob ();
};


namespace unopt 
{
  // { dg-final { scan-assembler ".extern .func _ZN5unopt3fooEv \\(.param.u64 %in_ar0\\);" } }
  A __attribute__ ((__optimize__ ("O0"))) foo ();

  // { dg-final { scan-assembler-times ".visible .func _ZN5unopt3barEv \\(.param.u64 %in_ar0\\)(?:;|\[\r\n\]+\{)" 2 } }
  A __attribute__ ((__optimize__ ("O0"), noinline)) bar()
  {
    A l;
    return l;
  }

  // { dg-final { scan-assembler-times ".visible .func _ZN5unopt3bazEv \\(.param.u64 %in_ar0\\)(?:;|\[\r\n\]+\{)" 2 } }
  A __attribute__ ((__optimize__ ("O0"), noinline)) baz ()
  {
    return foo ();
  }

  void __attribute__ ((__optimize__ ("O0"), noinline)) quux ()
  {
    bar ().frob ();
    baz ().frob ();
    foo ().frob ();
  }
  
}

namespace opt
{
  // { dg-final { scan-assembler ".extern .func _ZN3opt3fooEv \\(.param.u64 %in_ar0\\);" } }
  A __attribute__ ((__optimize__ ("O2"), noinline)) foo ();

  // { dg-final { scan-assembler-times ".visible .func _ZN3opt3barEv \\(.param.u64 %in_ar0\\)(?:;|\[\r\n\]+\{)" 2 } }
  A __attribute__ ((__optimize__ ("O2"), noinline)) bar()
  {
    A l;
    return l;
  }

  // { dg-final { scan-assembler-times ".visible .func _ZN3opt3bazEv \\(.param.u64 %in_ar0\\)(?:;|\[\r\n\]+\{)" 2 } }
  A __attribute__ ((__optimize__ ("O2"))) baz ()
  {
    return foo ();
  }

  void __attribute__ ((__optimize__ ("O2"), noinline)) quux ()
  {
    bar ().frob ();
    baz ().frob ();
    foo ().frob ();
  }
}

// Make sure we're not trying return a return value anywhere.
// { dg-final { scan-assembler-not "st.param.u64\[\t \]*\\\[%out_retval\\\], %\[_a-z0-9\]*;" } }
