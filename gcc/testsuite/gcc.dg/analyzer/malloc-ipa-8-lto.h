#include <stddef.h>

extern void *wrapped_malloc (size_t size);
extern void wrapped_free (void *ptr);

typedef struct boxed_int
{
  int i;
} boxed_int;

extern boxed_int *make_boxed_int (int i);
extern void free_boxed_int (boxed_int *bi);
