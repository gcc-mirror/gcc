/* PR middle-end/97391 - bogus -Warray-bounds accessing a multidimensional
   array parameter
   { dg-do compile }
   { dg-options "-O2 -Wall" } */


void nowarn_access_loop_idx (char a[3][5])
{
  for (int i = 0; i < 3; i++)
    for (int j = 0; j < 5; j++)
      a[i][j] = 0;
}

void warn_access_loop_idx (char a[3][5])
{
  for (int i = 0; i < 3; i++)
    for (int j = 0; j < 5; j++)
      a[j][i] = 0;            // { dg-warning "\\\[-Warray-bounds" }
}


void nowarn_access_cst_idx (int a[5][7][9])
{
  a[0][0][0] = __LINE__;
  a[0][0][8] = __LINE__;

  a[0][6][0] = __LINE__;
  a[0][6][8] = __LINE__;

  a[4][0][0] = __LINE__;
  a[4][0][8] = __LINE__;
  a[4][6][8] = __LINE__;
}


void test_ptr_access_cst_idx (int a[5][7][9])
{
  int *p = &a[0][0][0];

  p[0] = __LINE__;
  p[8] = __LINE__;

  /* The following access should trigger a warning but it's represented
     the same  as the valid access in
       p = a[0][1][0];
       p[1] = __LINE__;
     both as
       MEM[(int *)a_1(D) + 36B] = __LINE__;  */

  p[9] = __LINE__;            // { dg-warning "\\\[-Warray-bounds" "pr?????" { xfail *-*-* } }

  p[315] = __LINE__;
  // { dg-warning "subscript 315 is outside array bounds of 'int\\\[5]\\\[7]\\\[9]'" "pr97425" { xfail *-*-* } .-1 }
  // { dg-warning "subscript 315 is outside array bounds " "" { target *-*-* } .-2 }

  p = &a[0][6][0];
  p[0] = __LINE__;
  p[8] = __LINE__;

  p = &a[4][6][0];
  p[0] = __LINE__;
  p[8] = __LINE__;
}


void warn_access_cst_idx (int a[5][7][9])
{
  a[0][0][9] = __LINE__;      // { dg-warning "subscript 9 is above array bounds of 'int\\\[9]'" }
  a[0][7][0] = __LINE__;      // { dg-warning "subscript 7 is above array bounds of 'int\\\[7]\\\[9]'" }
  a[5][0][0] = __LINE__;
  // { dg-warning "subscript 5 is outside array bounds of 'int\\\[5]\\\[7]\\\[9]'" "pr97425" { xfail *-*-* } .-1 }
  // { dg-warning "subscript \\d+ is outside array bounds" "" { target *-*-* } .-2 }
}


void test_ptrarray_access_cst_idx (int (*pa)[5][7][9])
{
  (*pa)[0][0][0] = __LINE__;
  (*pa)[0][0][8] = __LINE__;
  (*pa)[0][0][9] = __LINE__;  // { dg-warning "subscript 9 is above array bounds of 'int\\\[9]'" }

  (*pa)[0][6][0] = __LINE__;
  (*pa)[0][7][0] = __LINE__;  // { dg-warning "subscript 7 is above array bounds of 'int\\\[7]\\\[9]'" }
  (*pa)[0][8][0] = __LINE__;  // { dg-warning "subscript 8 is above array bounds of 'int\\\[7]\\\[9]'" }

  (*pa)[4][6][8] = __LINE__;
  (*pa)[5][0][0] = __LINE__;  // { dg-warning "subscript 5 is above array bounds of 'int\\\[5]\\\[7]\\\[9]'" }
}


void test_ptr_ptrarray_access_cst_idx (int (*pa)[5][7][9])
{
  int *p = &(*pa)[0][0][0];

  p[0] = __LINE__;
  p[8] = __LINE__;

  /* The following access should trigger a warning but it's represented
     the same  as the valid access in
       p = a[0][1][0];
       p[1] = __LINE__;
     both as
       MEM[(int *)a_1(D) + 36B] = __LINE__;  */

  p[9] = __LINE__;            // { dg-warning "\\\[-Warray-bounds" "pr?????" { xfail *-*-* } }

  p[315] = __LINE__;          // { dg-warning "\\\[-Warray-bounds" "pr97429" { xfail *-*-* } }

  p = &(*pa)[0][6][0];
  p[0] = __LINE__;
  p[8] = __LINE__;

  p = &(*pa)[4][6][0];
  p[0] = __LINE__;
  p[8] = __LINE__;
}


