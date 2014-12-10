/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fselective-scheduling -fsel-sched-pipelining -fsel-sched-pipelining-outer-loops" } */

typedef enum
{
  empty = 0, pawn = 1, knight = 2, king = 3, bishop = 5, rook = 6, queen = 7
}
PIECE;
extern int p_values[15];
extern int *last[65];
int
Quiesce (int alpha, int beta, int wtm, int ply)
{
  register int initial_alpha, value, delta;
  register int *goodmv, *movep, moves = 0, *sortv, temp;
  for (movep = last[ply - 1]; movep < last[ply]; movep++)
    if (p_values[(((*movep) >> 15) & 7) + 7] +
        p_values[(((*movep) >> 18) & 7) + 7] >= delta)
      {
        register int done;
        register int *end = last[ply - 1] + moves - 1;
        do
          {
            done = 1;
            movep = last[ply - 1];
            for (; movep < end; movep++, sortv++)
              if (*sortv < *(sortv + 1))
                {
                  *(movep + 1) = temp;
                  done = 0;
                }
          }
        while (!done);
      }
}

