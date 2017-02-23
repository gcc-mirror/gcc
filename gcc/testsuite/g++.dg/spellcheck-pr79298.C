// Ensure that we can offer suggestions for misspellings via a
// namespace alias.

namespace N { int x; int color; }
namespace M = N; 
namespace O = M; 

int foo () 
{
  return M::y; // { dg-error ".y. is not a member of .M." }
}

int bar () 
{
  return O::colour; // { dg-error ".colour. is not a member of .O." }
  // { dg-message "suggested alternative: .color." "" { target *-*-* } .-1 }
}
