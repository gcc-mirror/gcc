#include <stdlib.h>

typedef short hashNx;

typedef struct hashSt {
  hashNx *hs_index;
  int hs_used;
  int hs_slots;
} hashSt;

int test (hashSt *td, int slots)
{
  hashNx *index;

  index = malloc((size_t)slots * sizeof(hashNx));
  if (index == NULL)
    return 0;
  td->hs_index = index;
  td->hs_slots = slots;

  for (slots = td->hs_slots, index = td->hs_index; --slots >= 0;)
    *index++ = -1;

  return 1;
}
