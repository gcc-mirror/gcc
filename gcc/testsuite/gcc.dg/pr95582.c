/* { dg-do compile } */
/* { dg-options "-fgimple -O3" } */

typedef _Bool bool8 __attribute__((signed_bool_precision(8)));

bool8 data[16];

void __GIMPLE(ssa) foo(int f)
{
  _Bool t;
  bool8 tp;

__BB(2):
   t_2 = f_1(D) != 0;
   tp_3 = (bool8) t_2;
   data[0] = tp_3;
   data[1] = tp_3;
   return;
}
