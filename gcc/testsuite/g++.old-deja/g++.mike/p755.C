#include <new>
void* operator new(size_t sz) throw (std::bad_alloc) {
typedef __SIZE_TYPE__ size_t;
void* operator new(size_t sz) {
