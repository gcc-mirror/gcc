#include <new>
void *operator new(size_t sz) throw (std::bad_alloc) {
