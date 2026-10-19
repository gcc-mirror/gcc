/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* A chain of four comparisons is wide enough for reassociation to build a
   balanced tree.  Both source shapes have to reach expansion as one chain,
   which a conditional compare sequence can hold in the flags.  */

/*
** and_chain:
**	cmp	w4, w5
**	ccmp	w6, w7, 0, lt
**	ccmp	w2, w3, 0, lt
**	ccmp	w0, w1, 0, lt
**	cset	w0, lt
**	ret
*/
int
and_chain (int a, int b, int c, int d, int e, int f, int g, int h)
{
  return (a < b) & (c < d) & (e < f) & (g < h);
}

/*
** and_tree:
**	cmp	w4, w5
**	ccmp	w6, w7, 0, lt
**	ccmp	w2, w3, 0, lt
**	ccmp	w0, w1, 0, lt
**	cset	w0, lt
**	ret
*/
int
and_tree (int a, int b, int c, int d, int e, int f, int g, int h)
{
  return ((a < b) & (c < d)) & ((e < f) & (g < h));
}

/*
** ior_tree:
**	cmp	w4, w5
**	ccmp	w6, w7, 1, ge
**	ccmp	w2, w3, 1, ge
**	ccmp	w0, w1, 1, ge
**	cset	w0, lt
**	ret
*/
int
ior_tree (int a, int b, int c, int d, int e, int f, int g, int h)
{
  return ((a < b) | (c < d)) | ((e < f) | (g < h));
}

/* Both operands hold a value in the flags at the same time, so this one
   needs two sequences and a bitwise operation.  */

int
and_of_ior (int a, int b, int c, int d, int e, int f, int g, int h)
{
  return ((a < b) | (c < d)) & ((e < f) | (g < h));
}

/* { dg-final { scan-assembler-times {\tand\tw[0-9]+, w[0-9]+, w[0-9]+} 1 } } */
