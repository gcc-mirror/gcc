/* PR middle-end/100574 - ICE: in size_remaining, at builtins.c:413 with
   -O3 -ftracer -fno-tree-dominator-opts -fno-tree-fre
   { dg-do compile { target c++11 } }
   { dg-options "-O3 -ftracer -fno-tree-dominator-opts -fno-tree-fre" } */

void *operator new (__SIZE_TYPE__, void *__p) { return __p; }

template <typename> struct allocator_traits;
template <typename> class allocator {};
template <typename _Tp> struct allocator_traits<allocator<_Tp> >
{
  using allocator_type = allocator<_Tp>;
  using pointer = _Tp *;
  using size_type = long;
  template <typename _Up> using rebind_alloc = allocator<_Up>;
  static pointer allocate(allocator_type, size_type);
  template <typename _Up> static void construct(_Up __p) { new (__p) _Up(); }
};

struct __alloc_traits : allocator_traits<allocator<char>> {
  struct rebind {
    typedef rebind_alloc<char> other;
  };
};

struct _Vector_base {
  struct : __alloc_traits::rebind::other {
  } _M_impl;
  long _M_allocate___n;
};

template <typename, typename = char> class vector : _Vector_base {
  long max_size();
public:
  void push_back() { _M_realloc_insert(); }
  template <typename...> void _M_realloc_insert();
};

template <typename _Tp, typename _Alloc>
template <typename...>
void vector<_Tp, _Alloc>::_M_realloc_insert() {
  __alloc_traits::pointer __trans_tmp_5;
  long __len(__len || max_size()), __elems_before;
  __trans_tmp_5 = _M_allocate___n
    ? __alloc_traits::allocate(_M_impl, _M_allocate___n)
    : __alloc_traits::pointer();
  __alloc_traits::construct(__trans_tmp_5 + __elems_before);
}

enum { MIDIST_PITCHBEND };
struct DataBlock {
  vector<char> data;
};

char ReadTrackChunk_status;

void ReadTrackChunk()
{
  DataBlock block;
  while (!0)
    switch (ReadTrackChunk_status)
    case MIDIST_PITCHBEND:
      block.data.push_back();
}
