/* { dg-do compile } */

struct bbro_basic_block_data {
  int priority;
  void *heap;
  void *node;
};
int array_size;
struct bbro_basic_block_data *bbd;
void reorder_basic_blocks_software_trace_cache(void)
{
  int i;
  for (i = 0; i < array_size; i++) {
    bbd[i].heap = (void *)0;
    bbd[i].node = (void *)0;
  }
}
