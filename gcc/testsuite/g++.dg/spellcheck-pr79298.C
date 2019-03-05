// Ensure that we can offer suggestions for misspellings via a
// namespace alias.
// { dg-options "-fdiagnostics-show-caret" }

namespace N { int x; int color; }
namespace M = N; 
namespace O = M; 

int foo () 
{
  return M::y; // { dg-error ".y. is not a member of .M." }
  /* { dg-begin-multiline-output "" }
   return M::y;
             ^
     { dg-end-multiline-output "" } */
}

int bar () 
{
  return O::colour; // { dg-error ".colour. is not a member of .O.; did you mean 'color'\\?" }
  /* { dg-begin-multiline-output "" }
   return O::colour;
             ^~~~~~
             color
     { dg-end-multiline-output "" } */
}
