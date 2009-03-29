struct obstack {};
struct bitmap_head_def;
typedef struct bitmap_head_def *bitmap;
typedef const struct bitmap_head_def *const_bitmap;
typedef unsigned long BITMAP_WORD;

typedef struct bitmap_obstack
{
  struct bitmap_element_def *elements;
  struct bitmap_head_def *heads;
  struct obstack obstack;
} bitmap_obstack;
typedef struct bitmap_element_def
{
  struct bitmap_element_def *next;
  struct bitmap_element_def *prev;
  unsigned int indx;
  BITMAP_WORD bits[(2)];
} bitmap_element;

struct bitmap_descriptor;

typedef struct bitmap_head_def {
    bitmap_element *first;
    bitmap_element *current;
    unsigned int indx;
    bitmap_obstack *obstack;
} bitmap_head;

bitmap_element bitmap_zero_bits;

typedef struct
{
  bitmap_element *elt1;
  bitmap_element *elt2;
  unsigned word_no;
  BITMAP_WORD bits;
} bitmap_iterator;

static __attribute__((noinline)) void
bmp_iter_set_init (bitmap_iterator *bi, const_bitmap map,
		   unsigned start_bit, unsigned *bit_no)
{
  bi->elt1 = map->first;
  bi->elt2 = ((void *)0);

  while (1)
    {
      if (!bi->elt1)
	{
	  bi->elt1 = &bitmap_zero_bits;
	  break;
	}

      if (bi->elt1->indx >= start_bit / (128u))
	break;
      bi->elt1 = bi->elt1->next;
    }

  if (bi->elt1->indx != start_bit / (128u))
    start_bit = bi->elt1->indx * (128u);

  bi->word_no = start_bit / 64u % (2);
  bi->bits = bi->elt1->bits[bi->word_no];
  bi->bits >>= start_bit % 64u;

  start_bit += !bi->bits;

  *bit_no = start_bit;
}

static __inline__ __attribute__((always_inline)) void
bmp_iter_next (bitmap_iterator *bi, unsigned *bit_no)
{
  bi->bits >>= 1;
  *bit_no += 1;
}

static __inline__ __attribute__((always_inline)) unsigned char
bmp_iter_set (bitmap_iterator *bi, unsigned *bit_no)
{
  if (bi->bits)
    {
      while (!(bi->bits & 1))
	{
	  bi->bits >>= 1;
	  *bit_no += 1;
	}
      return 1;
    }

  *bit_no = ((*bit_no + 64u - 1) / 64u * 64u);
  bi->word_no++;

  while (1)
    {
      while (bi->word_no != (2))
	{
	  bi->bits = bi->elt1->bits[bi->word_no];
	  if (bi->bits)
	    {
	      while (!(bi->bits & 1))
		{
		  bi->bits >>= 1;
		  *bit_no += 1;
		}
	      return 1;
	    }
	  *bit_no += 64u;
	  bi->word_no++;
	}

      bi->elt1 = bi->elt1->next;
      if (!bi->elt1)
	return 0;
      *bit_no = bi->elt1->indx * (128u);
      bi->word_no = 0;
    }
}

static void __attribute__((noinline))
foobar (bitmap_head *live_throughout)
{
  bitmap_iterator rsi;
  unsigned int regno;
  for (bmp_iter_set_init (&(rsi), (live_throughout), (0), &(regno));
       bmp_iter_set (&(rsi), &(regno));
       bmp_iter_next (&(rsi), &(regno)))
    ;
}
int main()
{
  bitmap_element elem = { (void *)0, (void *)0, 0, { 1, 1 } };
  bitmap_head live_throughout = { &elem, &elem, 0, (void *)0 };
  foobar (&live_throughout);
  return 0;
}

