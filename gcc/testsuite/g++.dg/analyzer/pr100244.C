// { dg-additional-options "-O1 -Wno-free-nonheap-object -Wno-analyzer-out-of-bounds" }
/* Disabled out-of-bounds checker because the output relied
   on optimizations.  out-of-bounds-placement-new.C tests
   the same pattern but without optimizations.  */

inline void *operator new (__SIZE_TYPE__, void *__p) { return __p; }

struct __aligned_buffer {
  int _M_storage;
  int *_M_addr() { return &_M_storage; }
};

struct _Hashtable_alloc {
  int _M_single_bucket;
  int *_M_buckets;
  _Hashtable_alloc () { _M_buckets = &_M_single_bucket; }
  ~_Hashtable_alloc () { delete _M_buckets; } // { dg-warning "on the stack" }
};

void
test01 (__aligned_buffer buf)
{
  _Hashtable_alloc *tmp = new (buf._M_addr ()) _Hashtable_alloc;
  tmp->~_Hashtable_alloc ();
}
