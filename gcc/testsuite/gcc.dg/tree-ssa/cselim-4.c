/* { dg-do compile } */
/* { dg-options "-O3 -fno-expensive-optimizations -fno-tree-dse -fgimple" } */

/* We're testing the CSELIM pass but we need VRP to compute the range
   for b_4 to make the array1[] accesses out-of-bound.  */

int array1[1];
int __GIMPLE (ssa,startwith("vrp"),guessed_local(1073741824))
f1 (int a, short unsigned int b1, int c)
{
  int b;

  __BB(2,guessed_local(1073741824)):
  b_3 = (int) b1_2(D);
  b_4 = b_3 + 1;
  if (a_5(D) != 0)
    goto __BB3(guessed(67108864));
  else
    goto __BB4(guessed(67108864));

  __BB(3,guessed_local(536870912)):
  array1[b_4] = c_7(D);
  array1[b_4] = c_7(D);
  goto __BB5(precise(134217728));

  __BB(4,guessed_local(536870912)):
  array1[b_4] = c_7(D);
  goto __BB5(precise(134217728));

  __BB(5,guessed_local(1073741824)):
  return;
}
