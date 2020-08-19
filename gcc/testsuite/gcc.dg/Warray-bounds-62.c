/* PR tree-optimization/84079 - missing -Warray-bounds taking the address
   of past-the-end element of a multidimensional array
   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0" } */

void sink (int, ...);

#define T(type, dims, inxs)			\
  do {						\
    type a dims;				\
    sink (__LINE__, &a inxs);			\
  } while (0)


void test_char_1_1 (int i0, int i1, int i2)
{
#undef DIMS
#define DIMS [1][1]

  T (char, DIMS, [0]);
  T (char, DIMS, [1]);
  T (char, DIMS, [2]);            // { dg-warning "subscript 2 is above array bounds of 'char\\\[1]\\\[1]'" }

  T (char, DIMS, [0][0]);
  T (char, DIMS, [0][1]);
  T (char, DIMS, [0][2]);         // { dg-warning "subscript 2 is above array bounds of 'char\\\[1]'" }

  T (char, DIMS, [1][0]);         // { dg-warning "subscript 1 is above array bounds of 'char\\\[1]\\\[1]'" }
  T (char, DIMS, [1][1]);         // { dg-warning "subscript 1 is above array bounds of 'char\\\[1]\\\[1]'" }
  T (char, DIMS, [1][2]);         // { dg-warning "subscript 2 is above array bounds of 'char\\\[1]'" }

  // Exercise ranges.
  if (i0 < 0) i0 = 0;
  if (i1 < 1) i1 = 1;
  if (i2 < 2) i2 = 2;

  T (char, DIMS, [i0]);
  T (char, DIMS, [i1]);
  T (char, DIMS, [i2]);           // { dg-warning "subscript 2 is above array bounds of 'char\\\[1]\\\[1]" }

  T (char, DIMS, [i0][i0]);
  T (char, DIMS, [i0][i1]);
  T (char, DIMS, [i1][i0]);       // { dg-warning "subscript 1 is above array bounds of 'char\\\[1]\\\[1]'" }
  T (char, DIMS, [i1][i1]);       // { dg-warning "subscript 1 is above array bounds of 'char\\\[1]\\\[1]'" }
  T (char, DIMS, [i1][i2]);       // { dg-warning "subscript 2 is above array bounds of 'char\\\[1]'" }
}


void test_int_3_5 (int i0, int i1, int i2, int i3, int i4, int i5, int i6)
{
#undef DIMS
#define DIMS [3][5]

  T (int, DIMS, [0]);
  T (int, DIMS, [3]);
  T (int, DIMS, [4]);             // { dg-warning "subscript 4 is above array bounds of 'int\\\[3]\\\[5]'" }

  T (int, DIMS, [0][0]);
  T (int, DIMS, [0][5]);
  T (int, DIMS, [0][6]);          // { dg-warning "subscript 6 is above array bounds of 'int\\\[5]'" }

  T (int, DIMS, [1][0]);
  T (int, DIMS, [1][5]);
  T (int, DIMS, [1][6]);          // { dg-warning "subscript 6 is above array bounds of 'int\\\[5]'" }

  T (int, DIMS, [3][0]);          // { dg-warning "subscript 3 is above array bounds of 'int\\\[3]\\\[5]'" }
  T (int, DIMS, [3][5]);          // { dg-warning "subscript 3 is above array bounds of 'int\\\[3]\\\[5]'" }
  T (int, DIMS, [3][6]);          // { dg-warning "subscript 6 is above array bounds of 'int\\\[5]'" }

  // Exercise ranges.
  if (i0 < 0) i0 = 0;
  if (i1 < 1) i1 = 1;
  if (i2 < 2) i2 = 2;
  if (i3 < 3) i3 = 3;
  if (i4 < 4) i4 = 4;
  if (i5 < 5) i5 = 5;
  if (i6 < 6) i6 = 6;

  T (int, DIMS, [i0]);
  T (int, DIMS, [i3]);
  T (int, DIMS, [i4]);            // { dg-warning "subscript 4 is above array bounds of 'int\\\[3]\\\[5]" }

  T (int, DIMS, [i0][i0]);
  T (int, DIMS, [i0][i5]);
  T (int, DIMS, [i0][i6]);        // { dg-warning "subscript 6 is above array bounds of 'int\\\[5]'" }

  T (int, DIMS, [i1][i0]);
  T (int, DIMS, [i1][i5]);
  T (int, DIMS, [i1][i6]);        // { dg-warning "subscript 6 is above array bounds of 'int\\\[5]'" }

  T (int, DIMS, [i3][i0]);        // { dg-warning "subscript 3 is above array bounds of 'int\\\[3]\\\[5]'" }
  T (int, DIMS, [i3][i5]);        // { dg-warning "subscript 3 is above array bounds of 'int\\\[3]\\\[5]'" }
  T (int, DIMS, [i3][i6]);        // { dg-warning "subscript 6 is above array bounds of 'int\\\[5]'" }
}


void test_int_2_3_4_5 (void)
{
#undef DIMS
#define DIMS [2][3][4][5]

  T (int, DIMS, [0]);
  T (int, DIMS, [2]);
  T (int, DIMS, [3]);             // { dg-warning "subscript 3 is above array bounds of 'int\\\[2]\\\[3]\\\[4]\\\[5]'" }

  T (int, DIMS, [0][0]);
  T (int, DIMS, [0][3]);
  T (int, DIMS, [0][4]);          // { dg-warning "subscript 4 is above array bounds of 'int\\\[3]\\\[4]\\\[5]'" }
  T (int, DIMS, [0][9]);          // { dg-warning "subscript 9 is above array bounds of 'int\\\[3]\\\[4]\\\[5]'" }

  T (int, DIMS, [0][0][0]);
  T (int, DIMS, [0][0][4]);
  T (int, DIMS, [0][0][5]);       // { dg-warning "subscript 5 is above array bounds of 'int\\\[4]\\\[5]'" }

  T (int, DIMS, [0][0][0][0]);
  T (int, DIMS, [0][0][0][5]);
  T (int, DIMS, [0][0][0][6]);    // { dg-warning "subscript 6 is above array bounds of 'int\\\[5]'" }

  T (int, DIMS, [0][0][1][0]);
  T (int, DIMS, [0][0][1][5]);
  T (int, DIMS, [0][0][1][6]);    // { dg-warning "subscript 6 is above array bounds of 'int\\\[5]'" }

  T (int, DIMS, [0][0][3][0]);
  T (int, DIMS, [0][0][3][5]);
  T (int, DIMS, [0][0][3][6]);    // { dg-warning "subscript 6 is above array bounds of 'int\\\[5]'" }

  T (int, DIMS, [0][0][1][0]);
  T (int, DIMS, [0][0][1][5]);
  T (int, DIMS, [0][0][1][6]);    // { dg-warning "subscript 6 is above array bounds of 'int\\\[5]'" }
}
