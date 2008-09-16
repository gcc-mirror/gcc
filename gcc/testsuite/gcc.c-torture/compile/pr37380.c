typedef struct basic_block_def *basic_block;
typedef struct gimple_seq_node_d *gimple_seq_node;
typedef struct gimple_seq_d *gimple_seq;
typedef struct
{
  gimple_seq_node ptr;
  gimple_seq seq;
  basic_block bb;
} gimple_stmt_iterator;
typedef void *gimple;
extern void exit(int);
struct gimple_seq_node_d
{
  gimple stmt;
  struct gimple_seq_node_d *next;
};
struct gimple_seq_d
{
};
static __inline__ gimple_stmt_iterator
gsi_start (gimple_seq seq)
{
  gimple_stmt_iterator i;
  i.seq = seq;
  return i;
}
static __inline__ unsigned char
gsi_end_p (gimple_stmt_iterator i)
{
  return i.ptr == ((void *)0);
}
static __inline__ void
gsi_next (gimple_stmt_iterator *i)
{
  i->ptr = i->ptr->next;
}
static __inline__ gimple
gsi_stmt (gimple_stmt_iterator i)
{
  return i.ptr->stmt;
}
void
c_warn_unused_result (gimple_seq seq)
{
  gimple_stmt_iterator i;
  for (i = gsi_start (seq); !gsi_end_p (i); gsi_next (&i))
    {
      gimple g = gsi_stmt (i);
      if (!g) exit(0);
    }
}
