/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom3" } */


typedef unsigned int size_t;
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
void
find_unreachable_blocks (int frobit)
{
  basic_block *tos, *worklist, bb;
  tos = worklist = xmalloc (sizeof (basic_block) * n_basic_blocks);
  edge e = frob();
  if (!(e->dest->flags & 4))
    {
      e->dest->flags |= 4;
      *tos++ = e->dest;
    }
}

/* If the aliasing code does its job properly, then we should be
   able to determine that modifying e->dest->flags does not
   modify e or e->dest.  The net result is that we only need one
   load of e->dest.  */
/* { dg-final { scan-tree-dump-times "->dest" 1 "dom3" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "dom3" } } */
