/* { dg-do assemble } */
/* { dg-options "-mcpu=5235 -Os" } */

typedef struct rtems_rfs_block_map_s
{
  long unsigned int blocks[(5)];
} rtems_rfs_block_map;

extern int foo (void);

int
rtems_rfs_block_map_indirect_alloc (rtems_rfs_block_map *map,
				    unsigned char* buffer, int b)
{
  (buffer + b * 4)[3] = (unsigned char) map->blocks[b];
}
