#include <new>
  friend void* operator new(size_t) throw (std::bad_alloc);
