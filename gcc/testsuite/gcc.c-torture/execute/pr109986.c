/* { dg-require-effective-target int32plus } */
/* PR middle-end/109986 */

#include "../../gcc.dg/tree-ssa/pr109986.c"

int 
main ()
{
  if (t1 (29789, 29477) != -28678) __builtin_abort ();
  if (t2 (20196, -18743) != 4294965567) __builtin_abort ();
  if (t3 (127, 99) != -100) __builtin_abort ();
  if (t4 (100, 53) != 219) __builtin_abort ();
  if (t5 (20100, 1283) != -1025) __builtin_abort ();
  if (t6 (20100, 10283) != 63487) __builtin_abort ();
  if (t7 (2136614690L, 1136698390L) != -1128276995L) __builtin_abort ();
  if (t8 (1136698390L, 2136614690L) != -1128276995UL) __builtin_abort ();
  if (t9 (9176690219839792930LL, 3176690219839721234LL) != -3175044472123688707LL)
    __builtin_abort ();
  if (t10 (9176690219839792930LL, 3176690219839721234LL) != 15271699601585862909ULL)
    __builtin_abort ();
  if (t11 (29789, 29477) != -28678) __builtin_abort ();
  if (t12 (20196, -18743) != 4294965567) __builtin_abort ();
  if (t13 (127, 99) != -100) __builtin_abort ();
  if (t14 (100, 53) != 219) __builtin_abort ();
  if (t15 (20100, 1283) != -1025) __builtin_abort ();
  if (t16 (20100, 10283) != 63487) __builtin_abort ();
  if (t17 (2136614690, 1136698390) != -1128276995) __builtin_abort ();
  if (t18 (1136698390L, 2136614690L) != -1128276995UL) __builtin_abort ();
  if (t19 (9176690219839792930LL, 3176690219839721234LL) != -3175044472123688707LL)
    __builtin_abort ();
  if (t20 (9176690219839792930LL, 3176690219839721234LL) != 15271699601585862909ULL)
    __builtin_abort ();
  v4si a1 = {1, 2, 3, 4};
  v4si a2 = {6, 7, 8, 9}; 
  v4si r1 = {-1, -3, -1, -1}; 
  v4si b1 = t21 (a1, a2);
  v4si b2 = t22 (a1, a2);
  if (__builtin_memcmp (&b1,  &r1,  sizeof (b1) != 0)) __builtin_abort();	
  if (__builtin_memcmp (&b2,  &r1,  sizeof (b2) != 0)) __builtin_abort();
  return 0;
}

