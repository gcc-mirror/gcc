/* Orgin: v.haisman@sh.cvut.cz
   Reduced by: Wolfgang Bangerth <bangerth@dealii.org>
   PR debug/12923  ICE in gen_subprogram_die with -O1 -g
   The problem was that this just to ICE with -O1  -g.  */

/* { dg-do compile } */
/* { dg-options "-O -g" } */

struct S {
  unsigned n;
};

inline void foo (struct S * mx) {
  mx->n = 1;
}

void bar () {
  foo (0);
}
