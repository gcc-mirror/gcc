// { dg-additional-options "-O1 -Wno-free-nonheap-object" }

inline void *operator new (__SIZE_TYPE__, void *__p) { return __p; }

struct __aligned_buffer {
  int _M_storage;
  int *_M_addr() { return &_M_storage; }
};

struct _Hashtable_alloc {
  int _M_single_bucket;
  int *_M_buckets;
  _Hashtable_alloc () { _M_buckets = &_M_single_bucket; }
  ~_Hashtable_alloc () { delete _M_buckets; } // { dg-warning "not on the heap" }
};

void
test01 (__aligned_buffer buf)
{
  _Hashtable_alloc *tmp = new (buf._M_addr ()) _Hashtable_alloc;
  tmp->~_Hashtable_alloc ();
}
