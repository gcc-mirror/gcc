/* { dg-do compile } */

extern short var_15, var_20;
#if __SIZEOF_INT__ >= 4
extern int var_18, var_21, var_23;
#else
extern __INT32_TYPE__ var_18, var_21, var_23;
#endif
extern _Bool arr_2[];
extern long arr_3[];
void test()
{
  var_20 = 1;
  for (int a = 0; a < 12; a += 2)
    for (short b = 0; b < 8; b += 2) {
      arr_2[b] = var_21 = var_18 ? var_15 : 0;
      arr_3[b] = 8569;
    }
  var_23 = -1096835496;
}
