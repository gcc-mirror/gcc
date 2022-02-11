/* { dg-additional-options "-Wno-analyzer-too-complex" } */

#include <stdlib.h>

typedef short hashNx;

typedef struct hashSt {
  hashNx *hs_index;
  int hs_used;
  int hs_slots;
} hashSt;

void hashEmpty(hashSt *td);

int hashAlloc(hashSt *td, int slots) {
  hashNx *index;

  if (slots > td->hs_slots) {
    if (td->hs_index != NULL)
      index = realloc(td->hs_index, (size_t)slots * sizeof(hashNx));
    else
      index = malloc((size_t)slots * sizeof(hashNx));

    if (index == NULL)
      return 0;

    td->hs_index = index;
    td->hs_slots = slots;
  }

  hashEmpty(td);

  return 1;
}

void hashEmpty(hashSt *td) {
  hashNx *index;
  int slots;

  for (slots = td->hs_slots, index = td->hs_index; --slots >= 0;)
    *index++ = -1;

  td->hs_used = 0;
}
