/* Reduced from analyzer ICE seen with git-2.39.0's pack-bitmap.c
   when bounds-checking the result of __builtin_ctzll.  */

#include <stdint.h>
#include <stddef.h>

typedef uint64_t eword_t;
struct ewah_bitmap;
struct ewah_iterator
{
  /* [...] */
};
struct bitmap;

void ewah_iterator_init(struct ewah_iterator *it, struct ewah_bitmap *parent);
int ewah_iterator_next(eword_t *next, struct ewah_iterator *it);
void bitmap_set(struct bitmap *self, size_t pos);

int rebuild_bitmap(const uint32_t *reposition,
     struct ewah_bitmap *source,
     struct bitmap *dest)
{
 uint32_t pos = 0;
 struct ewah_iterator it;
 eword_t word;

 ewah_iterator_init(&it, source);

 while (ewah_iterator_next(&word, &it)) {
  uint32_t offset, bit_pos;

  for (offset = 0; offset < (sizeof(eword_t) * 8); ++offset) {
   if ((word >> offset) == 0)
    break;

   offset += __builtin_ctzll(word >> offset);

   bit_pos = reposition[pos + offset];
   if (bit_pos > 0)
    bitmap_set(dest, bit_pos - 1);
   else
    return -1;
  }

  pos += (sizeof(eword_t) * 8);
 }
 return 0;
}
