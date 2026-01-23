/* { dg-do compile } */
/* { dg-options "-march=armv9-a -O3 -mmax-vectorization -msve-vector-bits=128 -mautovec-preference=sve-only -fdump-tree-vect-details" } */

struct partition_elem
{
  int class_element;
  struct partition_elem* next;
  unsigned class_count;
};

typedef struct partition_def
{
  int num_elements;
  struct partition_elem elements[1];
} *partition;

partition part;

partition
partition_new (int num_elements)
{
  int e;

  /* No need to allocate memory, just a compile test.  */
  for (e = 0; e < num_elements; ++e)
    {
      part->elements[e].class_element = e;
      part->elements[e].next = &(part->elements[e]);
      part->elements[e].class_count = 1;
    }

  return part;
}

/* { dg-final { scan-tree-dump-not "\\{ 0, 576 \\}" "vect" } } */
