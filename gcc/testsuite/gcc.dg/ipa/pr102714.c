/* { dg-do compile } */
/* { dg-options "-O2 -fno-strict-aliasing -fdump-ipa-sra-details -fdump-tree-optimized" } */

typedef _Bool bool;

enum {
 false = 0,
 true = 1
};

struct xarray {
 unsigned int xa_lock;
 unsigned int xa_flags;
 void * xa_head;

};

struct list_head {
 struct list_head *next, *prev;
};

struct callback_head {
 struct callback_head *next;
 void (*func)(struct callback_head *head);
} __attribute__((aligned(sizeof(void *))));

struct xa_node {
 unsigned char shift;
 unsigned char offset;
 unsigned char count;
 unsigned char nr_values;
 struct xa_node *parent;
 struct xarray *array;
 union {
  struct list_head private_list;
  struct callback_head callback_head;
 };
 void *slots[(1UL << (0 ? 4 : 6))];
 union {
  unsigned long tags[3][((((1UL << (0 ? 4 : 6))) + (64) - 1) / (64))];
  unsigned long marks[3][((((1UL << (0 ? 4 : 6))) + (64) - 1) / (64))];
 };
};

static inline __attribute__((__gnu_inline__)) __attribute__((__unused__)) __attribute__((no_instrument_function)) unsigned long shift_maxindex(unsigned int shift)
{
 return ((1UL << (0 ? 4 : 6)) << shift) - 1;
}

static inline __attribute__((__gnu_inline__)) __attribute__((__unused__)) __attribute__((no_instrument_function)) unsigned long node_maxindex(const struct xa_node *node)
{
 return shift_maxindex(node->shift);
}

static inline __attribute__((__gnu_inline__)) __attribute__((__unused__)) __attribute__((no_instrument_function)) struct xa_node *entry_to_node(void *ptr)
{
 return (void *)((unsigned long)ptr & ~2UL);
}

static inline __attribute__((__gnu_inline__)) __attribute__((__unused__)) __attribute__((no_instrument_function)) bool radix_tree_is_internal_node(void *ptr)
{
 return ((unsigned long)ptr & 3UL) ==
    2UL;
}

static inline __attribute__((__gnu_inline__)) __attribute__((__unused__)) __attribute__((no_instrument_function)) void *xa_mk_internal(unsigned long v)
{
 return (void *)((v << 2) | 2);
}

static unsigned radix_tree_load_root(const struct xarray *root,
  struct xa_node **nodep, unsigned long *maxindex)
{
 struct xa_node *node =
 ({
    typeof(root->xa_head) ________p1 = ({(*(const volatile typeof(root->xa_head) *)&(root->xa_head)); });
    ((typeof(*root->xa_head) *)(________p1));
 });

 *nodep = node;

 if (__builtin_expect(!!(radix_tree_is_internal_node(node)), 1)) {
  node = entry_to_node(node);
  *maxindex = node_maxindex(node);
  return node->shift + (0 ? 4 : 6);
 }

 *maxindex = 0;
 return 0;
}

void *__radix_tree_lookup(const struct xarray *root,
     unsigned long index, struct xa_node **nodep,
     void ***slotp)
{
 struct xa_node *node, *parent;
 unsigned long maxindex;

 restart:
 parent = ((void *)0);
 radix_tree_load_root(root, &node, &maxindex);
 while (radix_tree_is_internal_node(node)) {

  parent = entry_to_node(node);
  if (node == xa_mk_internal(256))
   goto restart;
  if (parent->shift == 0)
   break;
 }
 if (nodep)
  *nodep = parent;

 return node;
}

/* { dg-final { scan-ipa-dump-not "IPA_PARAM_OP_SPLIT" "sra" } } */
/* { dg-final { scan-tree-dump " ={v} " "optimized" } } */
