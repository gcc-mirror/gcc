/* { dg-do compile }  */
/* { dg-options "-Os" } */

typedef unsigned short __u16;
typedef unsigned int __u32;

typedef signed short s16;


static inline __attribute__((always_inline)) __attribute__((__const__)) __u16 __arch_swab16(__u16 x)
{
 __asm__(
  "swap.b		%1, %0"
  : "=r" (x)
  : "r" (x));
 return x;
}

void u16_add_cpu(__u16 *var)
{
  *var = __arch_swab16(*var);
}

typedef struct xfs_mount {
 int m_attr_magicpct;
} xfs_mount_t;

typedef struct xfs_da_args {
 struct xfs_mount *t_mountp;
 int index;
} xfs_da_args_t;

typedef struct xfs_dabuf {
 void *data;
} xfs_dabuf_t;

typedef struct xfs_attr_leaf_map {
 __u16 base;
 __u16 size;
} xfs_attr_leaf_map_t;
typedef struct xfs_attr_leaf_hdr {
 __u16 count;
 xfs_attr_leaf_map_t freemap[3];
} xfs_attr_leaf_hdr_t;

typedef struct xfs_attr_leaf_entry {
  __u16 nameidx;
} xfs_attr_leaf_entry_t;

typedef struct xfs_attr_leafblock {
 xfs_attr_leaf_hdr_t hdr;
 xfs_attr_leaf_entry_t entries[1];
} xfs_attr_leafblock_t;

int
xfs_attr_leaf_remove(xfs_attr_leafblock_t *leaf, xfs_da_args_t *args)
{
 xfs_attr_leaf_hdr_t *hdr;
 xfs_attr_leaf_map_t *map;
 xfs_attr_leaf_entry_t *entry;
 int before, after, smallest, entsize;
 int tablesize, tmp, i;
 xfs_mount_t *mp;
 hdr = &leaf->hdr;
 mp = args->t_mountp;

 entry = &leaf->entries[args->index];

 tablesize = __arch_swab16(hdr->count);

 map = &hdr->freemap[0];
 tmp = map->size;
 before = after = -1;
 smallest = 3 - 1;
 entsize = xfs_attr_leaf_entsize(leaf, args->index);

 for (i = 0; i < 2; map++, i++) {

  if (map->base == tablesize)
    u16_add_cpu(&map->base);

  if (__arch_swab16(map->base)  + __arch_swab16(map->size)  == __arch_swab16(entry->nameidx))
   before = i;
  else if (map->base == entsize)
   after = i;
  else if (__arch_swab16(map->size) < tmp)
   smallest = i;
 }

 if (before >= 0)
  {
   map = &hdr->freemap[after];
   map->base = entry->nameidx;

  }

  map = &hdr->freemap[smallest];

  map->base = __arch_swab16(entry->nameidx);

 return(tmp < mp->m_attr_magicpct);
}
