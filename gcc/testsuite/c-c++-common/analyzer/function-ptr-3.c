#include <stdlib.h>

typedef void *(*alloc_func_t) (size_t);
typedef void (*free_func_t) (void *);

typedef struct callbacks
{
  alloc_func_t alloc_cb;
  free_func_t dealloc_cb;
} callbacks_t;

void test (void)
{
  callbacks_t cb;
  cb.alloc_cb = (alloc_func_t)0;
  cb.dealloc_cb = (free_func_t)0;
}
