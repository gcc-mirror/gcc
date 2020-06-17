#include <stdlib.h>
#include "malloc-ipa-8-lto.h"

boxed_int *
make_boxed_int (int i)
{
  boxed_int *result = (boxed_int *)wrapped_malloc (sizeof (boxed_int));
  if (!result)
    abort ();
  result->i = i;
  return result;
}

void
free_boxed_int (boxed_int *bi)
{
  wrapped_free (bi);
}
