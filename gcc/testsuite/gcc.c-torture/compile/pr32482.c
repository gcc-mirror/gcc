typedef struct { unsigned long bits[((((1 << 0))+32 -1)/32)]; } nodemask_t;
static inline __attribute__((always_inline))
int bitmap_empty(const unsigned long *src, int nbits)
{
  return ! (*src & ( ((nbits) % 32) ? (1UL<<((nbits) % 32))-1 : ~0UL ));
}
static inline __attribute__((always_inline))
int __nodes_empty(const nodemask_t *srcp, int nbits)
{
 return bitmap_empty(srcp->bits, nbits);
}
extern nodemask_t node_online_map;
void drain_array(void);
void drain_cpu_caches(void)
{
 int node;
 if (!__nodes_empty(&(node_online_map), (1 << 0)))
     for (((node)) = 0; ((node)) < 1; ((node))++)
     {
     }
 if (!__nodes_empty(&(node_online_map), (1 << 0)))
     drain_array();
}
