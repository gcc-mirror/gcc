/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "maskeqz" 2 } }  */
/* { dg-final { scan-assembler-times "masknez" 2 } }  */

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
