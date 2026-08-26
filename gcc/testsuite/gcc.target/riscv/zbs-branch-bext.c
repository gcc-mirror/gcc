/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbs -mabi=lp64 -O2" { target rv64 } } */
/* { dg-options "-march=rv32gc_zbs -mabi=ilp32 -O2" { target rv32 } } */

/* Verify that masked bit positions in minimal inlined bitset tests are
   folded into BEXT for both branch polarities.  */

typedef __SIZE_TYPE__ size_t;

extern void sink (void);

#define BITS_PER_WORD (sizeof (unsigned long) * 8)

static inline _Bool
bitset_test (const unsigned long *words, size_t index)
{
  unsigned long word = words[index / BITS_PER_WORD];
  unsigned long mask = 1UL << (index % BITS_PER_WORD);
  return (word & mask) != 0;
}

void
branch_on_bit_set (const unsigned long *words, size_t index)
{
  if (bitset_test (words, index))
    sink ();
}

void
branch_on_bit_clear (const unsigned long *words, size_t index)
{
  if (!bitset_test (words, index))
    sink ();
}

/* { dg-final { scan-assembler-times {\mbext\t} 2 } } */
