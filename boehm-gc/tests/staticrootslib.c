#include <stdio.h>

#ifndef GC_DEBUG
# define GC_DEBUG
#endif

#include "gc.h"

struct treenode {
    struct treenode *x;
    struct treenode *y;
} * root[10];

struct treenode * libsrl_mktree(int i)
{
  struct treenode * r = GC_MALLOC(sizeof(struct treenode));
  if (0 == i) return 0;
  if (1 == i) r = GC_MALLOC_ATOMIC(sizeof(struct treenode));
  r -> x = libsrl_mktree(i-1);
  r -> y = libsrl_mktree(i-1);
  return r;
}

void * libsrl_init(void)
{
  GC_INIT();
  return GC_MALLOC(sizeof(struct treenode));
}

void * libsrl_collect (void)
{
  GC_gcollect();
}
