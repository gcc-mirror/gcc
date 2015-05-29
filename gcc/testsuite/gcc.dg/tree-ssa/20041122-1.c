/* { dg-do compile } */
/* { dg-options "-O1 -fstrict-aliasing -fdump-tree-cddce1" } */

__extension__ typedef __SIZE_TYPE__ size_t;
extern void *xmalloc (size_t) __attribute__ ((__malloc__));
struct edge_def
{
  struct basic_block_def *dest;
  int flags;
};
typedef struct edge_def *edge;
struct basic_block_def
{
  int flags;
};
typedef struct basic_block_def *basic_block;
extern int n_basic_blocks;
extern edge frob ();
basic_block *
find_unreachable_blocks (void)
{
  basic_block *tos, *worklist;
  tos = worklist = xmalloc (sizeof (basic_block) * n_basic_blocks);
  edge e = frob();
  if (!(e->dest->flags & 4))
    {
      e->dest->flags |= 4;
      *tos++ = e->dest;
    }
  return worklist;
}

/* If the aliasing code does its job properly, then we should be
   able to determine that modifying e->dest->flags does not
   modify e or e->dest if we can assert strict-aliasing rules.
   The net result is that we only need one load of e->dest.  */
/* { dg-final { scan-tree-dump-times "->dest" 1 "cddce1" } } */
