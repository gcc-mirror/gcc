/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
extern const unsigned long base;
static inline void wreg(unsigned char val, unsigned long addr) __attribute__((always_inline));
static inline void wreg(unsigned char val, unsigned long addr)
{
   *((volatile unsigned char *) (__SIZE_TYPE__) (base + addr)) = val;
}
void wreg_twice(void)
{
   wreg(0, 42);
   wreg(0, 42);
}

/* We should not remove the second null character store to (base+42) address. */
/* { dg-final { scan-tree-dump-times " ={v} 0;" 2 "optimized" } }  */
/* { dg-final { cleanup-tree-dump "optimized" } }  */
