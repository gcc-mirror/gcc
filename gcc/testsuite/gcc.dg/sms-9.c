/* { dg-do run } */
/* { dg-options "-O2 -fmodulo-sched -fmodulo-sched-allow-regmoves" } */

#include <stdlib.h>
#include <stdarg.h>

struct df_ref_info
{
  unsigned int *begin;
  unsigned int *count;
};

extern void *memset (void *s, int c, __SIZE_TYPE__ n);


__attribute__ ((noinline))
int
df_reorganize_refs_by_reg_by_insn (struct df_ref_info *ref_info,
			           int num, unsigned int start)
{
  unsigned int m = num;
  unsigned int offset = 77;
  unsigned int r;

  for (r = start; r < m; r++)
    {
      ref_info->begin[r] = offset;
      offset += ref_info->count[r];
      ref_info->count[r] = 0;
    }

  return offset;
}

int
main ()
{
  struct df_ref_info temp;
  int num = 100;
  unsigned int start = 5;
  int i, offset;

  temp.begin = malloc (100 * sizeof (unsigned int));
  temp.count = malloc (100 * sizeof (unsigned int));

  memset (temp.begin, 0, sizeof (unsigned int) * num);
  memset (temp.count, 0, sizeof (unsigned int) * num);

  for (i = 0; i < num; i++)
    temp.count[i] = i + 1;

  offset = df_reorganize_refs_by_reg_by_insn (&temp, num, start);

  if (offset != 5112)
    abort ();

  free (temp.begin);
  free (temp.count);
  return 0;
}
