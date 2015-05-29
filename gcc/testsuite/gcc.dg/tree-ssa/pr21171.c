/* PR 21171.  Ivopts should not rewrite references to volatile memory.  */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

typedef	unsigned int u_int32_t;
typedef	unsigned char u_int8_t;

#define	AIC_VECTORS	32

typedef volatile struct AT91RM9200_regs {
  u_int32_t	SVR[AIC_VECTORS];
} AT91RM9200_regs_t;

#define CPUReg  ((AT91RM9200_regs_t*)0xFFF00000)

extern const u_int32_t __IntTable[AIC_VECTORS];

int main()
{
  int c;

  for (c = 0; c < AIC_VECTORS; c++)
    CPUReg->SVR[c] = __IntTable[c];

  return 0;
}

/* { dg-final { scan-tree-dump-times "SVR" 1 "optimized"} } */
