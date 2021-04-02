// { dg-additional-options "-fmodules-ts -Wno-pedantic" }

module;
# 4 __FILE__ 1
inline void frob ()
{
  extern int bob; // OK
}

# 11 "" 2
export module bob;
// { dg-module-cmi !bob }

inline void dob ()
{
  extern int bob; // { dg-error "block-scope extern" }
}


// { dg-prune-output "not writing module" }
