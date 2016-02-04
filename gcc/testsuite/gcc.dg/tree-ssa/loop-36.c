/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dce3" } */

struct X { float array[2]; };

struct X a,b;

float foobar () {
  float s = 0;
  unsigned int d;
  struct X c;
  for (d=0; d<2; ++d)
    c.array[d] = a.array[d] * b.array[d];
  for (d=0; d<2; ++d)
    s+=c.array[d];
  return s;
}

/* The temporary structure should have been promoted to registers
   by FRE after the loops have been unrolled by the early unrolling pass.  */
/* { dg-final { scan-tree-dump-not "c\.array" "dce3" } } */
