//Build don't link:
struct cl_heap_ring{
    void operator delete (void* ptr) { }
    cl_heap_ring ()
    { }
};

struct cl_heap_null_ring : public cl_heap_ring {
    void operator delete (void* ptr) { }
};

void f()
{
  new cl_heap_null_ring();
}
