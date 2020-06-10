/* { dg-do run } */
/* { dg-additional-sources "pr95295-2.c" } */

extern int var_4, a;
extern unsigned var_9;
extern short arr_272[];
void test()
{
  for (int b = 0; b < 9; b++)
    for (int c = 0; c < 9; c += 4)
      {
	arr_272[c] = var_9 ? var_4 : 0;
	a = 0;
      }
}
