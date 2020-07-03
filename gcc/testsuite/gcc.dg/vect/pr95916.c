/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

extern short var_3, var_8;
extern int var_5;
extern char var_10;
extern int arr_99[][16];
void test()
{
  for (; 0 < var_10;)
    for (long a = var_8;; a++)
      arr_99[4][a] = var_3 << var_5;
}
