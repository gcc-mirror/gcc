/* PR tree-optimization/106495 */
/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds -fdump-tree-threadfull1-details" } */

/* Reduced from PR106495 (attachment 53396, comment #6): the i686 bootstrap
   broke with -Werror=array-bounds.  The calls to the cold noreturn fancy_abort
   clobber memory, so m_vec is reloaded for each access; the backward threader
   can then resolve the m_vec == NULL path, where vec_length () is 0 and the
   index wraps to 4294967294, and isolate it even though it ends in a probably
   never executed edge.  Nothing real ever executes the path, but the
   materialized dead statements drew a bogus -Warray-bounds warning (ilp32
   only; the threadfull1 scan below discriminates on all targets).

   The PR106495 fix rejects such paths in profitable_path_p: this test FAILs
   before that commit and PASSes with it.  cvise-reduced from the original .ii
   file, and then the AI converted the C++ vec templates to plain C.  See
   pr106495-2.c for a minimal distillation.  */

void fancy_abort (const char *, int, const char *)
  __attribute__ ((noreturn)) __attribute__ ((cold));

typedef int *basic_block;

unsigned m_num;

struct vec_embed
{
  int m_pad;
  basic_block m_vecdata[];
};

struct vec
{
  struct vec_embed *m_vec;
};

static inline unsigned
embed_length (struct vec_embed *v)
{
  return m_num;
}

static inline basic_block
embed_index (struct vec_embed *v, unsigned ix)
{
  if (!(ix < m_num))
    fancy_abort ("", 9, __FUNCTION__);
  return v->m_vecdata[ix];	/* { dg-bogus "above array bounds" } */
}

static inline unsigned
vec_length (struct vec *v)
{
  return v->m_vec ? embed_length (v->m_vec) : 0;
}

static inline basic_block
vec_index (struct vec *v, unsigned ix)
{
  return embed_index (v->m_vec, ix);
}

void find_edge (basic_block, basic_block);

struct vec m_path;

void
profitable_path_p (void)
{
  int len1 = vec_length (&m_path);
  unsigned len2 = vec_length (&m_path);
  basic_block bb = vec_index (&m_path, len1 - 2);
  find_edge (vec_index (&m_path, len2 - 1), bb);
}

/* { dg-final { scan-tree-dump "path leads to probably never executed edge" "threadfull1" } } */
