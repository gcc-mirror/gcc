/* { dg-add-shlib "staticrootslib.c" } */

#include <stdio.h>

#ifndef GC_DEBUG
# define GC_DEBUG
#endif

#include "gc.h"
#include "gc_backptr.h"

struct treenode {
    struct treenode *x;
    struct treenode *y;
} * root[10];

static char *staticroot = 0;

extern struct treenode * libsrl_mktree(int i);
extern void * libsrl_init(void);
extern void * libsrl_collect (void);

int main(void)
{
  int i;
  staticroot = libsrl_init();
  for (i = 0; i < sizeof(struct treenode); ++i) {
    staticroot[i] = 0x42;
  }
  libsrl_collect();
  for (i = 0; i < 10; ++i) {
    root[i] = libsrl_mktree(12);
    libsrl_collect();
  }
  for (i = 0; i < sizeof(struct treenode); ++i) {
    if (staticroot[i] != 0x42)
      return -1;
  }
  for (i = 0; i < 10; ++i) {
    root[i] = libsrl_mktree(12);
    libsrl_collect();
  }
  for (i = 0; i < sizeof(struct treenode); ++i) {
    if (staticroot[i] != 0x42)
      return -1;
  }
  return 0;
}
