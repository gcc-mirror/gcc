/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zicond_zbb -mabi=lp64d" } */
/* { dg-skip-if "" { *-*-* } { "-finline-functions" "-funroll-loops" "-ftracer" } } */

int
remove_one_fast (int *move_ordering, const int num_moves, int mark)
{
  int i, best = -1000000;
  int tmp = 0;

  for (i = mark; i < num_moves; i++)
    {
      if (move_ordering[i] > best)
        {
          best = move_ordering[i];
          tmp = i;
        }
    }

  return tmp;
}

/* { dg-final { scan-assembler-times "max\t" 1 } }  */
/* { dg-final { scan-assembler-times "czero.nez" 2 } }  */
/* { dg-final { scan-assembler-times "czero.eqz" 2 } }  */

int
remove_one_fast2 (int *move_ordering, const int num_moves, int mark)
{
  int i, best = -1000000;
  int tmp = 0;

  for (i = mark; i < num_moves; i++)
    {
      if (move_ordering[i] < best)
        {
          best = move_ordering[i];
          tmp = i;
        }
    }

  return tmp;
}

/* { dg-final { scan-assembler-times "min\t" 1 } }  */
