/* { dg-options "-O2 -fdump-tree-optimized" } */

#define __GFP_DMA 1u
#define __GFP_RECLAIM 0x10u

#define KMALLOC_DMA 2
#define KMALLOC_RECLAIM 1

unsigned int
imul(unsigned int flags)
{
  int is_dma, type_dma, is_rec;

  is_dma = !!(flags & __GFP_DMA);
  type_dma = is_dma * KMALLOC_DMA;
  is_rec = !!(flags & __GFP_RECLAIM);

  return type_dma + (is_rec * !is_dma) * KMALLOC_RECLAIM;
}

/* { dg-final { scan-tree-dump-times { \*w? |WIDEN_MULT_PLUS_EXPR} 1 "optimized" } } */
