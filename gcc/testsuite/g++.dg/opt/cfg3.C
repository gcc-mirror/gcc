// PR optimization/11646
// Origin: <nick@ilm.com>

// This used to fail because the compiler inadvertently cleared
// the EDGE_ABNORMAL flag on a EDGE_EH edge and didn't delete
// unreachable blocks after CSE.

// { dg-do compile }
// { dg-options "-O -fgcse -fnon-call-exceptions" }

struct C
{
  int i;
};

struct allocator
{
  ~allocator() throw() {}
};

struct _Vector_alloc_base
{
  _Vector_alloc_base(const allocator& __a) {}
  allocator _M_data_allocator;
  struct C *_M_start, *_M_end_of_storage;
  void _M_deallocate(struct C* __p, unsigned int __n) {}
};

struct _Vector_base : _Vector_alloc_base
{
  _Vector_base(const allocator& __a) : _Vector_alloc_base(__a) { }
  ~_Vector_base() { _M_deallocate(0, _M_end_of_storage - _M_start); }
};

struct vector : _Vector_base
{
  vector(const allocator& __a = allocator()) : _Vector_base(__a) {}
  struct C& operator[](unsigned int __n) { return *_M_start; }
};

struct A
{
  float l() const;
  A operator-(const A &) const;
  const A& operator=(float) const;
};

struct B
{
  float d();
};

float f(const A& a, B& b)
{
  vector vc;
  int index = vc[0].i;
  A aa;
  float d = (aa - a).l();
  if (d > b.d()) aa = 0;
    return b.d();
}
