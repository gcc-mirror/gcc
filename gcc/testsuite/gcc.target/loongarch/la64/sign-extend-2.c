/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -O2 -fdump-rtl-expand" } */
/* { dg-final { scan-rtl-dump "subreg/s" "expand" } } */
/* { dg-final { scan-assembler-not "slli.w\t\\\$r\[0-9\]+,\\\$r\[0-9\]+,0" } } */

#include <stdint.h>
#define my_min(x, y) ((x) < (y) ? (x) : (y))

void
bt_skip_func (const uint32_t len_limit, const uint32_t pos,
              const uint8_t *const cur, uint32_t cur_match,
              uint32_t *const son, const uint32_t cyclic_pos,
              const uint32_t cyclic_size)
{
  uint32_t *ptr0 = son + (cyclic_pos << 1) + 1;
  uint32_t *ptr1 = son + (cyclic_pos << 1);

  uint32_t len0 = 0;
  uint32_t len1 = 0;

  while (1)
    {
      const uint32_t delta = pos - cur_match;
      uint32_t *pair
          = son
            + ((cyclic_pos - delta + (delta > cyclic_pos ? cyclic_size : 0))
               << 1);
      const uint8_t *pb = cur - delta;
      uint32_t len = my_min (len0, len1);

      if (pb[len] == cur[len])
        {
          while (++len != len_limit)
            if (pb[len] != cur[len])
              break;

          if (len == len_limit)
            {
              *ptr1 = pair[0];
              *ptr0 = pair[1];
              return;
            }
        }

      if (pb[len] < cur[len])
        {
          *ptr1 = cur_match;
          ptr1 = pair + 1;
          cur_match = *ptr1;
          len1 = len;
        }
      else
        {
          *ptr0 = cur_match;
          ptr0 = pair;
          cur_match = *ptr0;
          len0 = len;
        }
    }
}
