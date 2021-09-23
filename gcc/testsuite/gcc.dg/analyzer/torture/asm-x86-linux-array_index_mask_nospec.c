/* { dg-do assemble { target x86_64-*-* } } */
/* { dg-require-effective-target lp64 } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

#include "../analyzer-decls.h"

/* Copied from linux: arch/x86/include/asm/barrier.h (GPL-2.0) */

static inline unsigned long array_index_mask_nospec(unsigned long index,
						    unsigned long size)
{
	unsigned long mask;

	asm volatile ("cmp %1,%2; sbb %0,%0;"
			:"=r" (mask)
			:"g"(size),"r" (index)
			:"cc");
	return mask;
}

/* The analyzer ought to treat array_index_mask_nospec as being
   effectively pure.  */

void test_1 (unsigned long index, unsigned long size)
{
  unsigned long a = array_index_mask_nospec (index, size);
  unsigned long b = array_index_mask_nospec (index, size);
  __analyzer_eval (a == b); /* { dg-warning "TRUE" } */
}

void test_2 (unsigned long index_a, unsigned long size_a,
	     unsigned long index_b, unsigned long size_b)
{
  unsigned long aa_1 = array_index_mask_nospec (index_a, size_a);
  unsigned long ab_1 = array_index_mask_nospec (index_a, size_b);
  unsigned long ba_1 = array_index_mask_nospec (index_b, size_a);
  unsigned long bb_1 = array_index_mask_nospec (index_b, size_b);

  unsigned long aa_2 = array_index_mask_nospec (index_a, size_a);
  unsigned long ab_2 = array_index_mask_nospec (index_a, size_b);
  unsigned long ba_2 = array_index_mask_nospec (index_b, size_a);
  unsigned long bb_2 = array_index_mask_nospec (index_b, size_b);

  __analyzer_eval (aa_1 == aa_2); /* { dg-warning "TRUE" } */
  __analyzer_eval (ab_1 == ab_2); /* { dg-warning "TRUE" } */
  __analyzer_eval (ba_1 == ba_2); /* { dg-warning "TRUE" } */
  __analyzer_eval (bb_1 == bb_2); /* { dg-warning "TRUE" } */

  __analyzer_eval (aa_1 == ab_1); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (aa_1 == ba_1); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (aa_1 == bb_1); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (ab_1 == ba_1); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (ab_1 == bb_1); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (ba_1 == bb_1); /* { dg-warning "UNKNOWN" } */
}

/* Equivalent asm strings should be treated the same, rather
   than requiring the results to come from the same stmt.  */

void test_3 (unsigned long index, unsigned long size)
{
  unsigned long a = array_index_mask_nospec (index, size);
  unsigned long b;

  /* Copy of the asm from array_index_mask_nospec.  */
  asm volatile ("cmp %1,%2; sbb %0,%0;"
		:"=r" (b)
		:"g"(size),"r" (index)
		:"cc");

  __analyzer_eval (a == b); /* { dg-warning "TRUE" } */
}
