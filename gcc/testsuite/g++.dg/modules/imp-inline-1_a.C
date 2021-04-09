// { dg-additional-options "-fmodules-ts -Wno-pedantic" }

module;

# 6 __FILE__ 1
struct Bob 
{
  // inline
  static auto frob () 
  {
  }
};

# 14 "" 2

export module Foo;
// { dg-module-cmi Foo }

export struct Bill 
{
  // not inline
  static auto dob () 
  {
  }
  static inline auto frob () 
  {
  }
};

export inline auto GMF ()
{
  return Bob::frob ();
}

// { dg-final { scan-assembler-not {_ZN3Bob4frobEv:} } }
// { dg-final { scan-assembler-not {_ZN4Bill4frobEv:} } }
// { dg-final { scan-assembler {_ZN4Bill3dobEv:} } }
