/* Execution test for conditional-compare chains built from AND/IOR trees of
   comparisons.  Each function is compared against a reference computed with
   volatile operands so that it cannot be folded into the same code.  */

extern void abort (void);

#define OPS int a, int b, int c, int d, int e, int f, int g, int h
#define ARGS a, b, c, d, e, f, g, h

/* Balanced AND tree.  */
static int __attribute__((noipa)) t1 (OPS)
{ return ((a < b) & (c < d)) & ((e < f) & (g < h)); }

/* Balanced OR tree.  */
static int __attribute__((noipa)) t2 (OPS)
{ return ((a < b) | (c < d)) | ((e < f) | (g < h)); }

/* AND of an OR chain and an AND chain.  */
static int __attribute__((noipa)) t3 (OPS)
{ return ((a < b) | (c < d)) & ((e < f) & (g < h)); }

/* OR of an AND chain and an OR chain.  */
static int __attribute__((noipa)) t4 (OPS)
{ return ((a < b) & (c < d)) | ((e < f) | (g < h)); }

/* Deeper: eight leaves.  */
static int __attribute__((noipa)) t5 (OPS)
{
  return (((a < b) & (c < d)) & ((e < f) & (g < h)))
	 & (((a < c) & (b < d)) & ((e < g) & (f < h)));
}

/* Mixed signed and unsigned leaves.  */
static int __attribute__((noipa)) t6 (OPS)
{
  return (((unsigned) a < (unsigned) b) & (c < d))
	 & ((e < f) & ((unsigned) g < (unsigned) h));
}

/* Immediates, some inside and some outside the CCMP 5-bit range.  */
static int __attribute__((noipa)) t7 (OPS)
{ return ((a == 3) & (b == 31)) & ((c == 32) & (d == -4)); }

/* Reference versions.  V forces separate evaluation of every leaf.  */
#define V(x) ({ volatile int v_ = (x); v_; })

static int r1 (OPS)
{ return (V(a < b) & V(c < d)) & (V(e < f) & V(g < h)); }
static int r2 (OPS)
{ return (V(a < b) | V(c < d)) | (V(e < f) | V(g < h)); }
static int r3 (OPS)
{ return (V(a < b) | V(c < d)) & (V(e < f) & V(g < h)); }
static int r4 (OPS)
{ return (V(a < b) & V(c < d)) | (V(e < f) | V(g < h)); }
static int r5 (OPS)
{
  return ((V(a < b) & V(c < d)) & (V(e < f) & V(g < h)))
	 & ((V(a < c) & V(b < d)) & (V(e < g) & V(f < h)));
}
static int r6 (OPS)
{
  return (V((unsigned) a < (unsigned) b) & V(c < d))
	 & (V(e < f) & V((unsigned) g < (unsigned) h));
}
static int r7 (OPS)
{ return (V(a == 3) & V(b == 31)) & (V(c == 32) & V(d == -4)); }
static const int vals[] = { -4, 0, 3, 31, 32, 33 };
#define NV ((int) (sizeof (vals) / sizeof (vals[0])))

int
main (void)
{
  for (int i0 = 0; i0 < NV; i0++)
    for (int i1 = 0; i1 < NV; i1++)
      for (int i2 = 0; i2 < NV; i2++)
	for (int i3 = 0; i3 < NV; i3++)
	  {
	    int a = vals[i0], b = vals[i1], c = vals[i2], d = vals[i3];
	    int e = vals[i3], f = vals[i0], g = vals[i2], h = vals[i1];

	    if (t1 (ARGS) != r1 (ARGS)) abort ();
	    if (t2 (ARGS) != r2 (ARGS)) abort ();
	    if (t3 (ARGS) != r3 (ARGS)) abort ();
	    if (t4 (ARGS) != r4 (ARGS)) abort ();
	    if (t5 (ARGS) != r5 (ARGS)) abort ();
	    if (t6 (ARGS) != r6 (ARGS)) abort ();
	    if (t7 (ARGS) != r7 (ARGS)) abort ();
	  }
  return 0;
}
