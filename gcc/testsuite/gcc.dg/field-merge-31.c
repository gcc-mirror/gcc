/* { dg-do run } */
/* { dg-options "-O1 -fdump-tree-ifcombine-details" } */

/* Check that compares with different masks on each side are still merged
   when the merged compare tests each bit the same way as the originals,
   and that they are not merged when it would not.  */

struct S { unsigned char x, y; } __attribute__ ((aligned (4))) s, t;

#define F(n, e)							\
  __attribute__ ((noipa)) int n (void) { return (e) ? 1 : 0; }	\
  __attribute__ ((noipa, optimize ("O0")))			\
  int n##_ref (void) { return (e) ? 1 : 0; }

/* Merged: the fields don't overlap.  */
F (f1, s.x == (t.x & 0x7f) && s.y == t.y)
/* Merged: the second compare is implied by the first.  */
F (f2, s.x == (t.x & 0x7f) && (s.x & 0xf) == (t.x & 0xf))
/* Not merged: bit 7 is tested for zero in s.x by the first compare, and
   for equality by the second.  */
F (f3, s.x == (t.x & 0x7f) && (s.x & 0x80) == (t.x & 0x80))
/* Not merged: bits 4 and 5 are tested for equality by the first compare,
   and for zero in t.x by the second.  */
F (f4, (s.x & 0x3c) == (t.x & 0x3c) && (s.x & 0xf0) == t.x)

int
main ()
{
  for (int a = 0; a < 256; a++)
    for (int b = 0; b < 256; b++)
      for (int y = 0; y < 2; y++)
	{
	  s.x = a; t.x = b; s.y = 3; t.y = 3 + y;
	  if (f1 () != f1_ref () || f2 () != f2_ref ()
	      || f3 () != f3_ref () || f4 () != f4_ref ())
	    __builtin_abort ();
	}
  return 0;
}

/* { dg-final { scan-tree-dump-times "optimizing two comparisons" 2 "ifcombine" { target { ! { avr-*-* pru-*-* } } } } } */
