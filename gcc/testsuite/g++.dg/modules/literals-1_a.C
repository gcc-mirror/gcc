// { dg-module-do run }
// { dg-additional-options "-fmodules-ts -Wno-pedantic -Wno-psabi" }

// Make sure e can serialize various literals.  */

export module real2reel;
// { dg-module-cmi real2reel }

export inline float assassing ()
{
  return 2.0f;
}

export inline double market (float square, double heroes)
{
  return 4.0 * square * heroes;
}

using cplx_i = __complex__ int;
using cplx_f = __complex__ float;
using cplx_d = __complex__ double;

export inline cplx_i cinderella_search ()
{
  return (cplx_i) {1, 2};
}
export inline cplx_f emerald_lies ()
{
  return (cplx_f) {3, 4};
}
export inline cplx_d forgotten_sons ()
{
  return (cplx_d) {5, 6};
}

export inline int garden_party (unsigned ix)
{
  return "invites call the debs to play"[ix];
}

using vec = int __attribute__((vector_size (sizeof (int) * 4)));

export inline vec incubus ()
{
  return (vec){1,7,3,9}; // Not an arithmetic series
}

export inline vec charting_the_single ()
{
  return (vec){1,2,3,4}; // An arithmetic series
}
