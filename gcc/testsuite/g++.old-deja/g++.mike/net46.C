#include <new>
static void *operator new(size_t size) throw (std::bad_alloc) {
static void *operator new(size_t size) {
