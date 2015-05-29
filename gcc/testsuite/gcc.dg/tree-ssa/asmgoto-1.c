/* { dg-do compile } */
/* { dg-options "-O2 -w -fdump-tree-optimized" } */

extern void XYZZY (void);
typedef unsigned long __kernel_size_t;
typedef __kernel_size_t size_t;
typedef unsigned gfp_t;
struct per_cpu_pageset { } __attribute__ ((__aligned__ ((1 << (6)))));
struct zone { struct per_cpu_pageset *pageset[64]; }
zone_flags_t; typedef struct pglist_data { struct zone node_zones[4]; } pg_data_t;
extern struct pglist_data *first_online_pgdat (void);
extern struct zone *next_zone (struct zone *zone);
extern volatile int per_cpu__x86_cpu_to_node_map[];
struct kmem_cache { int size; };
extern struct kmem_cache kmalloc_caches[(12 + 2)];
struct tracepoint { void **funcs; } __attribute__ ((aligned (32)));
extern struct tracepoint __tracepoint_kmalloc_node;
void *__kmalloc_node (size_t size, gfp_t flags, int node);

static inline int
cpu_to_node (int cpu)
{
  return per_cpu__x86_cpu_to_node_map[cpu];
}

static inline void
trace_kmalloc_node (unsigned long call_site, const void *ptr,
		    size_t bytes_req, size_t bytes_alloc, gfp_t gfp_flags,
		    int node)
{
  asm goto ("" : : : : trace_label);
  if (0)
    {
	  void **it_func;
    trace_label:
	  asm ("" : "=r"(it_func) : "0"(&__tracepoint_kmalloc_node.funcs));
    }
};

static inline __attribute__ ((always_inline)) int
kmalloc_index (size_t size)
{
  if (size <= 64)
    return 6;
  return -1;
}

static inline __attribute__ ((always_inline)) struct kmem_cache *
kmalloc_slab (size_t size)
{
  int index = kmalloc_index (size);
  if (index == 0)
    return ((void *) 0);
  return &kmalloc_caches[index];
}

static inline __attribute__ ((always_inline)) void *
kmalloc_node (size_t size, gfp_t flags, int node)
{
  void *ret;
  if (__builtin_constant_p (size) && size <= (2 * ((1UL) << 12))
      && !(flags & ((gfp_t) 0x01u)))
    {
      struct kmem_cache *s = kmalloc_slab (size);
      if (!s)
	return ((void *) 16);
      trace_kmalloc_node (({ __here:(unsigned long) &&__here;}),
			  ret, size, s->size, flags, node);
    }
  return __kmalloc_node (size, flags, node);
}

int
process_zones (int cpu)
{
  struct zone *zone, *dzone;
  int node = cpu_to_node (cpu);
  for (zone = (first_online_pgdat ())->node_zones;
       zone; zone = next_zone (zone))
      {
	((zone)->pageset[(cpu)]) =
	  kmalloc_node (sizeof (struct per_cpu_pageset),
			(((gfp_t) 0x10u) | ((gfp_t) 0x40u) | ((gfp_t) 0x80u)),
			node);
	if (!((zone)->pageset[(cpu)]))
	  goto bad;
      }
  return 0;
bad:
  XYZZY ();
  return -12;
}

/* { dg-final { scan-tree-dump-times "XYZZY" 1 "optimized" } } */
