/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include <stdint.h>
#include <string.h>
#include "mpx-check.h"

#ifdef __i386__
/* i386 directory size is 4MB.  */
#define MPX_NUM_L2_BITS 10
#define MPX_NUM_IGN_BITS 2
#else /* __i386__ */
/* x86_64 directory size is 2GB.  */
#define MPX_NUM_L2_BITS 17
#define MPX_NUM_IGN_BITS 3
#endif /* !__i386__ */


/* bt_num_of_elems is the number of elements in bounds table.  */
unsigned long bt_num_of_elems = (1UL << MPX_NUM_L2_BITS);
/* Function to test MPX wrapper of memmove function.
   src_bigger_dst determines which address is bigger, can be 0 or 1.
   src_bt_index and dst_bt index are bt_indexes
   from the beginning of the page.
   bd_index_end is the bd index of the last element of src if we define
   bd index of the first element as 0.
   src_bt index_end is bt index of the last element of src.
   pointers inside determines if array being copied includes pointers
   src_align and dst_align are alignments of src and dst.
   Arrays may contain unaligned pointers.  */
int
test (int src_bigger_dst, int src_bt_index, int dst_bt_index,
      int bd_index_end, int src_bt_index_end, int pointers_inside,
      int src_align, int dst_align)
{
  const int n =
    src_bt_index_end - src_bt_index + bd_index_end * bt_num_of_elems;
  if (n < 0)
    {
      return 0;
    }
  const int num_of_pointers = (bd_index_end + 2) * bt_num_of_elems;
  void **arr = 0;
  posix_memalign ((void **) (&arr),
           1UL << (MPX_NUM_L2_BITS + MPX_NUM_IGN_BITS),
           num_of_pointers * sizeof (void *));
  void **src = arr, **dst = arr;
  if ((src_bigger_dst) && (src_bt_index < dst_bt_index))
    src_bt_index += bt_num_of_elems;
  if (!(src_bigger_dst) && (src_bt_index > dst_bt_index))
    dst_bt_index += bt_num_of_elems;
  src += src_bt_index;
  dst += dst_bt_index;
  char *realign = (char *) src;
  realign += src_align;
  src = (void **) realign;
  realign = (char *) dst;
  realign += src_align;
  dst = (void **) realign;
  if (pointers_inside)
    {
      for (int i = 0; i < n; i++)
        src[i] = __bnd_set_ptr_bounds (arr + i, i * sizeof (void *) + 1);
    }
  memmove (dst, src, n * sizeof (void *));
  if (pointers_inside)
    {
      for (int i = 0; i < n; i++)
        {
          if (dst[i] != arr + i)
            abort ();
          if (__bnd_get_ptr_lbound (dst[i]) != arr + i)
            abort ();
          if (__bnd_get_ptr_ubound (dst[i]) != arr + 2 * i)
            abort ();
        }
    }
  free (arr);
  return 0;
}

/* Call testall to test common cases of memmove for MPX.  */
void
testall ()
{
  int align[3];
  align[0] = 0;
  align[1] = 1;
  align[2] = 7;
  for (int pointers_inside = 0; pointers_inside < 2; pointers_inside++)
    for (int src_bigger_dst = 0; src_bigger_dst < 2; src_bigger_dst++)
      for (int src_align = 0; src_align < 3; src_align ++)
        for (int dst_align = 0; dst_align < 3; dst_align ++)
          for (int pages = 0; pages < 4; pages++)
            {
              test (src_bigger_dst, 1, 2, pages, 1, pointers_inside,
                    align[src_align], align[dst_align]);
              test (src_bigger_dst, 1, 2, pages, 2, pointers_inside,
                    align[src_align], align[dst_align]);
              test (src_bigger_dst, 2, 1, pages, 12, pointers_inside,
                    align[src_align], align[dst_align]);
              test (src_bigger_dst, 2, 1, pages, 1, pointers_inside,
                    align[src_align], align[dst_align]);
              test (src_bigger_dst, 2, 3, pages, 12, pointers_inside,
                    align[src_align], align[dst_align]);
              test (src_bigger_dst, 1, bt_num_of_elems - 2, pages, 2,
                    pointers_inside, align[src_align], align[dst_align]);
            }
};

int
mpx_test (int argc, const char **argv)
{
  testall ();
  return 0;
}
