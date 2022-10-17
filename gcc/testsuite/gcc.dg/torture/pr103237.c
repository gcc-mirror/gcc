/* { dg-do run } */
/* { dg-additional-options "-ftree-vectorize" } */

int g1;
unsigned int g2 = -1U;
static void __attribute__((noipa))
func_1()
{
  int *l_1 = &g1;
  for (int g3a = 0; g3a != 4; g3a++)
    for (int l_2 = 0; l_2 <= 3; l_2++)
      {
        unsigned int *l_3 = &g2;
        *l_1 = *l_3 ^= 1;
      }
}
int
main()
{
  func_1();
  if (g1 != -1)
    __builtin_abort ();
  return 0;
}
