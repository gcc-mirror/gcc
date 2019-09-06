/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized-raw" } */

static inline unsigned deposit32(unsigned value, int start, int length,
                                 unsigned fieldval)
{
    unsigned mask = (~0U >> (32 - length)) << start;
    return (value & ~mask) | ((fieldval << start) & mask);
}

unsigned foo(unsigned value)
{
   return deposit32(value, 10, 1, 1);
}

/* { dg-final { scan-tree-dump-not "bit_and_expr" "optimized" } } */
/* { dg-final { scan-tree-dump-not "bit_xor_expr" "optimized" } } */
/* { dg-final { scan-tree-dump-not "bit_not_expr" "optimized" } } */
