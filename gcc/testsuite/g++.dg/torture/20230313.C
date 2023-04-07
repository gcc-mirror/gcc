/* { dg-do run } */
/* { dg-additional-options "-fno-exceptions -fno-rtti" } */

extern "C" void free (void *);
void fancy_abort () { __builtin_abort (); }
struct vec_prefix
{
  unsigned m_alloc : 1;
  unsigned m_using_auto_storage : 1;
  unsigned m_num;
};
struct vl_ptr
;
struct va_heap
{
  typedef vl_ptr default_layout;
};
template<typename ,
         typename A ,
         typename = typename A::default_layout>
struct vec
;
template<typename T, typename A>
struct vec<T, A, int>
{
  T & operator[] (unsigned ix)
    {
      int *__trans_tmp_2;
      !m_vecpfx.m_num ? fancy_abort (), 0 : 0;
      __trans_tmp_2 =  reinterpret_cast <T *> (this + 1);
      return __trans_tmp_2[ix];
    }
  bool iterate (unsigned , T *ptr) {
      *ptr = 0;
      return false;
  }
  void embedded_init (unsigned , unsigned num, unsigned aut)
    {
      m_vecpfx.m_alloc =
	  m_vecpfx.m_using_auto_storage = aut;
      m_vecpfx.m_num = num;
    }
  void quick_grow (unsigned len)
    {
      !m_vecpfx.m_alloc ? fancy_abort (), 0 : m_vecpfx.m_num = len;
    }
  vec_prefix m_vecpfx;
};
template<typename T, int N >
struct auto_vec : vec<T, va_heap>
{
  auto_vec ()
  {
    m_auto.embedded_init (N, 0, 1);
    long off = (char *) &m_auto - (char *) this;
    this->m_vec = (vec<T, va_heap, int> *) ((char *) this + off);
  }
  ~auto_vec ()
  {
    this->release ();
  }
  vec<T, va_heap, int> m_auto;
  char m_data[2 * sizeof (int)];
};
template<typename T>
struct vec<T, va_heap>
{
void
release ()
{
  bool __trans_tmp_1 =  m_vec ? m_vec->m_vecpfx.m_using_auto_storage : false;
  if (__trans_tmp_1)
      return;
  vec<int, va_heap, int> *&v = m_vec;
    free (m_vec);
    v = nullptr;
}
T &operator[] (unsigned ix) { return (*m_vec)[ix]; }
bool
iterate (unsigned ix, T *ptr) {
    m_vec->iterate (ix, ptr);
    return false;
}
void
quick_grow (unsigned len)
{
  m_vec->quick_grow (len);
}
  vec<T, va_heap, int> *m_vec;
};
void test_auto_alias ()
{
  volatile int i = 1;
  auto_vec<int, 8> v;
  v.quick_grow (2);
  v[0] = 1;
  v[1] = 2;
  int val;
  for (int ix = i; v.iterate (ix, &val); ix++)
    if (val != 2)
      __builtin_abort ();
  if (val != 0)
    __builtin_abort ();
}
int main()
{
  test_auto_alias ();
  return 0;
}
