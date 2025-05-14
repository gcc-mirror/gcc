/* { dg-do compile } */
/* { dg-options "-march=rv64gcb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

struct obstack;
struct bitmap_head_def;
typedef struct bitmap_head_def *bitmap;
struct obstack
{
  long chunk_size;
  struct _obstack_chunk *chunk;
  char *object_base;
  char *next_free;
  char *chunk_limit;
  long int temp;
  int alignment_mask;



  struct _obstack_chunk *(*chunkfun) (void *, long);
  void (*freefun) (void *, struct _obstack_chunk *);
  void *extra_arg;
  unsigned use_extra_arg:1;
  unsigned maybe_empty_object:1;



  unsigned alloc_failed:1;


};

typedef unsigned long BITMAP_WORD;
typedef struct bitmap_obstack {
  struct bitmap_element_def *elements;
  struct bitmap_head_def *heads;
  struct obstack obstack;
} bitmap_obstack;
typedef struct bitmap_element_def {
  struct bitmap_element_def *next;
  struct bitmap_element_def *prev;
  unsigned int indx;
  BITMAP_WORD bits[((128 + (8 
                  * 8 * 1u) - 1) / (8 
                  * 8 * 1u))];
} bitmap_element;
bitmap_element *bitmap_find_bit (bitmap, unsigned int);


int
bitmap_bit_p (bitmap head, int bit)
{
  bitmap_element *ptr;
  unsigned bit_num;
  unsigned word_num;

  ptr = bitmap_find_bit (head, bit);
  if (ptr == 0)
    return 0;

  bit_num = bit % (8 
                 * 8 * 1u);
  word_num = bit / (8 
                  * 8 * 1u) % ((128 + (8 
                                     * 8 * 1u) - 1) / (8 
                                     * 8 * 1u));

  return (ptr->bits[word_num] >> bit_num) & 1;
}

/* { dg-final { scan-assembler-times "bext\t" 1 } } */
/* { dg-final { scan-assembler-not "slr\t"} } */
/* { dg-final { scan-assembler-not "andi\t"} } */

