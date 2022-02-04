// { dg-do compile }
// { dg-additional-options "-Wall" }

void* operator new[](unsigned long, void* __p);

struct allocator
{
  ~allocator();
};

void *foo (void *p)
{
  return p ? new(p) allocator[1] : new allocator[1]; // { dg-bogus "uninitialized" }
}
