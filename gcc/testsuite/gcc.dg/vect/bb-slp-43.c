/* { dg-do compile } */

void
f (int *restrict x, short *restrict y)
{
  x[0] = x[0] == 1 & y[0] == 2;
  x[1] = x[1] == 1 & y[1] == 2;
  x[2] = x[2] == 1 & y[2] == 2;
  x[3] = x[3] == 1 & y[3] == 2;
  x[4] = x[4] == 1 & y[4] == 2;
  x[5] = x[5] == 1 & y[5] == 2;
  x[6] = x[6] == 1 & y[6] == 2;
  x[7] = x[7] == 1 & y[7] == 2;
}

/* { dg-final { scan-tree-dump-not "mixed mask and nonmask" "slp2" } } */
/* { dg-final { scan-tree-dump-not "vector operands from scalars" "slp2" { target { { vect_int && vect_bool_cmp } && { vect_unpack && vect_hw_misalign } } xfail { vect_variable_length && { { ! vect128 } && { ! vect256 } } } } } } */
