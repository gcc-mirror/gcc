#include <stdlib.h>

struct ret
{
  int **array;
};

struct ret *allocate_stuff(void)
{
  struct ret *ret;

  ret = calloc(1, sizeof (struct ret));
  if (!ret) {
    abort();
  }

  ret->array = calloc (10, sizeof(int *));
  if (!ret->array) {
    abort();
  }

  return ret;
}
