/* { dg-do compile } */

typedef unsigned long long __u64;
static __u64 ext2_max_sizes[16 - 10 + 1];

void e2fsck_pass1()
{
 int i;
 __u64 max_sizes;

 for (i = 10; i <= 16; i++) {
  max_sizes = 12 + (1ULL << ((i) - 2));
  max_sizes = max_sizes + (1ULL << ((i) - 2)) * (1ULL << ((i) - 2));
  max_sizes = max_sizes + (1ULL << ((i) - 2)) * (1ULL << ((i) - 2)) * (1ULL <<((i) - 2));
  ext2_max_sizes[i - 10] = max_sizes;
 }
}

/* { dg-final { cleanup-tree-dump "vect" } } */
