/* We have encountered an issue that a "mov_s.ne" instruction *
 * with  an  immediate  value  was put in the delay slot of a *
 * branch:                                                    *
 *                                                            *
 * bne.d @.L1      # 33    [c=20 l=4]  *branch_insn           *
 * mov_s.ne r0,7   # 35    [c=0 l=6]  *movsi_ne/2             *
 *                                                            *
 * This is not sanctioned and must not happen. The test below *
 * is a reduced version of the source  code  leading  to  the *
 * problem.                                                   */

/* { dg-do compile }               */
/* { dg-skip-if "" { ! { clmcpu } } } */
/* { dg-options "-mcpu=archs -Og" } */
typedef struct
{
  struct
  {
    int length;
  } table;
} room;

struct house
{
  room *r;
};

int glob;

_Bool bar();

int func(struct house *h, int i, int whatever)
{
  for (;;)
  {
    _Bool a;
    if (h && h->r[i].table.length == glob)
    {
      if (whatever)
      {
        a = bar();
        if (__builtin_expect(!a, 0))
          return 7;
      }
      break;
    }
  }
  return 0;
}

/* no 'mov_s.ne r,limm' in a delay slot */
/* { dg-final { scan-assembler-not "bne.d\.*\n\\s\+mov_s.ne\\s+r\[0-9\]+,7" } } */
