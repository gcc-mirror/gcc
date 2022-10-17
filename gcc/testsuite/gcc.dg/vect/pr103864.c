/* { dg-do compile } */
/* { dg-additional-options "-O3 -fno-tree-reassoc" } */

void
crash_me (short int *crash_me_result, int i, char crash_me_ptr_0)
{
  while (i < 1)
    {
      int j;

      for (j = 0; j < 2; ++j)
        crash_me_result[j] += crash_me_ptr_0 + 1;

      i += 3;
    }
}
