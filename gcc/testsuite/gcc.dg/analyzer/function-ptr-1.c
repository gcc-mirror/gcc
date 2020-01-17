#include <stdlib.h>

typedef void *(*fn_ptr_t) (void *);

void *test_1 (fn_ptr_t fn_ptr, void *data)
{
  return fn_ptr (data);
}
