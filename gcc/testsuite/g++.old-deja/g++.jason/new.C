#include <new>
void * operator new (size_t siz) throw (std::bad_alloc) {
