/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse2-details" } */

typedef __SIZE_TYPE__ size_t;
extern "C"
{
  extern void *memmove (void *__dest, const void *__src, size_t __n) throw ()
    __attribute__ ((__nonnull__ (1, 2)));
}
extern void abort () __attribute__ ((__noreturn__));
struct vec_prefix { unsigned m_num; };
struct vl_embed { };
struct vl_ptr { };
struct va_heap { typedef vl_ptr default_layout; };
template < typename T, typename A = va_heap, typename L = typename A::default_layout > struct vec { };
template < typename T, typename A > struct vec < T, A, vl_embed >
{
  unsigned length (void) const { return m_vecpfx.  m_num; }
  bool is_empty (void) const { return m_vecpfx.  m_num == 0; }
  void block_remove (unsigned, unsigned);
  vec_prefix m_vecpfx;
  T m_vecdata[1];
};
template < typename T, typename A > inline void vec < T, A, vl_embed >::block_remove (unsigned ix, unsigned len)
{
  T * slot = &m_vecdata[ix];
  m_vecpfx.m_num -= len;
  memmove (slot, slot + len, (m_vecpfx.m_num - ix) * sizeof (T));
}

template < typename T > struct vec < T, va_heap, vl_ptr >
{
  bool is_empty (void) const { return m_vec ?  m_vec-> is_empty () : true; }
  unsigned length (void) const { return m_vec ?  m_vec-> length () : 0; }
  void block_remove (unsigned, unsigned);
  vec < T, va_heap, vl_embed > * m_vec;
};
template < typename T > inline void vec < T, va_heap, vl_ptr >::block_remove (unsigned ix, unsigned len)
{
  m_vec->block_remove (ix, len);
}

typedef struct _list_node * _list_t;
typedef struct _expr expr_def;
typedef expr_def * expr_t;
typedef _list_t av_set_t;
static vec < expr_t > vec_av_set;
bool
fill_vec_av_set (av_set_t av)
{
  if (vec_av_set.length () > 0)
    vec_av_set.block_remove (0, vec_av_set.length ());
  ((!(vec_av_set.is_empty ())? abort () , 0 : 0));
}

/* { dg-final { scan-tree-dump-not "Trimming statement .head = -" "dse2" } } */
/* { dg-final { scan-tree-dump "Deleted dead call: " "dse2" } } */


