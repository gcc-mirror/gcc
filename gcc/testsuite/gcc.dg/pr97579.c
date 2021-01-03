/* { dg-do compile } */
/* { dg-options "-O3 --param=max-unswitch-insns=1024" } */
/* { dg-additional-options "-mavx512vl" { target x86_64-*-* i?86-*-* } } */

int bad_odd_rows_0_0, rows_bad_row1, rows_bad_group_okay, calc_rows_row2;

int
rows_bad() {
  int i, in_zeroes;
  char block;
  i = 0;
  for (; i < 5; i++)
    if (rows_bad_row1 & i)
      in_zeroes = 0;
    else {
      if (!in_zeroes)
        in_zeroes = 1;
      if (block & 1)
        rows_bad_group_okay = 1;
    }
  if (in_zeroes)
    return rows_bad_group_okay;
}

void
calc_rows() {
  for (; calc_rows_row2; calc_rows_row2++) {
    rows_bad();
    bad_odd_rows_0_0 = rows_bad();
  }
}
