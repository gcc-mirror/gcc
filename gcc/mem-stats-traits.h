#ifndef GCC_MEM_STATS_TRAITS_H
#define GCC_MEM_STATS_TRAITS_H

/* Memory allocation origin.  */
enum mem_alloc_origin
{
  HASH_TABLE_ORIGIN,
  HASH_MAP_ORIGIN,
  HASH_SET_ORIGIN,
  VEC_ORIGIN,
  BITMAP_ORIGIN,
  GGC_ORIGIN,
  ALLOC_POOL_ORIGIN,
  MEM_ALLOC_ORIGIN_LENGTH
};

/* Verbose names of the memory allocation origin.  */
static const char * mem_alloc_origin_names[] = { "Hash tables", "Hash maps",
  "Hash sets", "Heap vectors", "Bitmaps", "GGC memory", "Allocation pool" };

#endif // GCC_MEM_STATS_TRAITS_H
