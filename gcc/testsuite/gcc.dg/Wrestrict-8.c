/* PR tree-optimization/84095 - false-positive -Wrestrict warnings for
   memcpy within array
   { dg-do compile }
   { dg-options "-O2 -Wrestrict -ftrack-macro-expansion=0" } */

typedef __SIZE_TYPE__ size_t;

extern void* memcpy (void* restrict, const void* restrict, size_t);

#define T(d, s, n)   memcpy (d, s, n)

struct S1 { char c; } a8_1[8];

void test_1_dim_var (int i, int j)
{
  /* Variable destination index and constant source index.  */
  T (&a8_1[i], &a8_1[0], 1);
  T (&a8_1[i], &a8_1[0], 2);
  T (&a8_1[i], &a8_1[0], 3);
  T (&a8_1[i], &a8_1[0], 4);

  T (&a8_1[i], &a8_1[0], 5);    /* { dg-warning "accessing 5 bytes at offsets \\\[0, 8] and 0 overlaps between 2 and 5 bytes at offset \\\[0, 3\\\]" } */
  T (&a8_1[i], &a8_1[0], 6);    /* { dg-warning "accessing 6 bytes at offsets \\\[0, 8] and 0 overlaps between 4 and 6 bytes at offset \\\[0, 2\\\]" } */
  T (&a8_1[i], &a8_1[0], 7);    /* { dg-warning "accessing 7 bytes at offsets \\\[0, 8] and 0 overlaps between 6 and 7 bytes at offset \\\[0, 1\\\]" } */
  T (&a8_1[i], &a8_1[0], 8);    /* { dg-warning "accessing 8 bytes at offsets \\\[0, 8] and 0 overlaps 8 bytes at offset 0" } */

  /* The following is diagnosed by -Warray-bounds when it's enabled
     rather than by -Wrestrict.  */
  T (&a8_1[i], &a8_1[0], 9);    /* { dg-warning "accessing 9 bytes at offsets \\\[0, 8] and 0 overlaps 9 bytes at offset 0" } */

  /* Same as above but with constant destination index and variable
     source index.  */
  T (&a8_1[0], &a8_1[i], 1);
  T (&a8_1[0], &a8_1[i], 2);
  T (&a8_1[0], &a8_1[i], 3);
  T (&a8_1[0], &a8_1[i], 4);

  T (&a8_1[0], &a8_1[i], 5);    /* { dg-warning "accessing 5 bytes at offsets 0 and \\\[0, 8] overlaps between 2 and 5 bytes at offset \\\[0, 3\\\]" } */
  T (&a8_1[0], &a8_1[i], 6);    /* { dg-warning "accessing 6 bytes at offsets 0 and \\\[0, 8] overlaps between 4 and 6 bytes at offset \\\[0, 2\\\]" } */
  T (&a8_1[0], &a8_1[i], 7);    /* { dg-warning "accessing 7 bytes at offsets 0 and \\\[0, 8] overlaps between 6 and 7 bytes at offset \\\[0, 1\\\]" } */
  T (&a8_1[0], &a8_1[i], 8);    /* { dg-warning "accessing 8 bytes at offsets 0 and \\\[0, 8] overlaps 8 bytes at offset 0" } */
  T (&a8_1[0], &a8_1[i], 9);    /* { dg-warning "accessing 9 bytes at offsets 0 and \\\[0, 8] overlaps 9 bytes at offset 0" } */


  /* Variable destination and source indices.  */
  T (&a8_1[i], &a8_1[j], 1);
  T (&a8_1[i], &a8_1[j], 2);
  T (&a8_1[i], &a8_1[j], 3);
  T (&a8_1[i], &a8_1[j], 4);

  T (&a8_1[i], &a8_1[j], 5);    /* { dg-warning "accessing 5 bytes at offsets \\\[0, 8] and \\\[0, 8] overlaps between 2 and 5 bytes at offset \\\[0, 3\\\]" } */
  T (&a8_1[i], &a8_1[j], 6);    /* { dg-warning "accessing 6 bytes at offsets \\\[0, 8] and \\\[0, 8] overlaps between 4 and 6 bytes at offset \\\[0, 2\\\]" } */
  T (&a8_1[i], &a8_1[j], 7);    /* { dg-warning "accessing 7 bytes at offsets \\\[0, 8] and \\\[0, 8] overlaps between 6 and 7 bytes at offset \\\[0, 1\\\]" } */
  T (&a8_1[i], &a8_1[j], 8);    /* { dg-warning "accessing 8 bytes at offsets \\\[0, 8] and \\\[0, 8] overlaps 8 bytes at offset 0" } */

  /* The following is diagnosed by -Warray-bounds when it's enabled
     rather than by -Wrestrict.  */
  T (&a8_1[i], &a8_1[j], 9);    /* { dg-warning "accessing 9 bytes at offsets \\\[0, 8] and \\\[0, 8] overlaps 9 bytes at offset 0" } */
}

struct S4 { char a4[4]; } a2_4[2];

void test_2_dim (int i, int j)
{
  T (&a2_4[i], &a2_4[0], 1);
  T (&a2_4[i], &a2_4[0], 4);

  T (&a2_4[i], &a2_4[0], 5);    /* { dg-warning "accessing 5 bytes at offsets \\\[0, 8] and 0 overlaps between 2 and 5 bytes at offset \\\[0, 3]" } */
  T (&a2_4[i], &a2_4[0], 6);    /* { dg-warning "accessing 6 bytes at offsets \\\[0, 8] and 0 overlaps between 4 and 6 bytes at offset \\\[0, 2]" } */
  T (&a2_4[i], &a2_4[0], 7);    /* { dg-warning "accessing 7 bytes at offsets \\\[0, 8] and 0 overlaps between 6 and 7 bytes at offset \\\[0, 1]" } */
  T (&a2_4[i], &a2_4[0], 8);    /* { dg-warning "accessing 8 bytes at offsets \\\[0, 8] and 0 overlaps 8 bytes at offset 0" } */

  T (a2_4[i].a4, a2_4[0].a4, 1);
  T (a2_4[i].a4, a2_4[0].a4, 4);

  T (a2_4[i].a4, a2_4[0].a4, 5);   /* { dg-warning "accessing 5 bytes at offsets \\\[0, 8] and 0 overlaps between 2 and 5 bytes at offset \\\[0, 3]" } */
  T (a2_4[i].a4, a2_4[0].a4, 8);   /* { dg-warning "accessing 8 bytes at offsets \\\[0, 8] and 0 overlaps 8 bytes at offset 0" } */

  T (a2_4[i].a4, a2_4[j].a4, 1);
  T (a2_4[i].a4, a2_4[j].a4, 4);

  /* The destination and source offsets printed below ignore the size
     of the copy and only indicate the values that are valid for each
     of the destination and source arguments on its own, without
     considering the size of the overlapping access.  */
  T (a2_4[i].a4, a2_4[j].a4, 5);   /* { dg-warning "accessing 5 bytes at offsets \\\[0, 8] and \\\[0, 8] overlaps between 2 and 5 bytes at offset \\\[0, 3]" } */
  T (a2_4[i].a4, a2_4[j].a4, 8);   /* { dg-warning "accessing 8 bytes at offsets \\\[0, 8] and \\\[0, 8] overlaps 8 bytes at offset 0" } */

  /* Same as above but referencing the first elements of each array.  */
  T (&a2_4[i].a4[0], &a2_4[j].a4[0], 1);
  T (&a2_4[i].a4[0], &a2_4[j].a4[0], 4);

  T (&a2_4[i].a4[0], &a2_4[j].a4[0], 5);   /* { dg-warning "accessing 5 bytes at offsets \\\[0, 8] and \\\[0, 8] overlaps between 2 and 5 bytes at offset \\\[0, 3]" } */
  T (&a2_4[i].a4[0], &a2_4[j].a4[0], 8);   /* { dg-warning "accessing 8 bytes at offsets \\\[0, 8] and \\\[0, 8] overlaps 8 bytes at offset 0" } */

  T (&a2_4[i].a4[0], &a2_4[j].a4[1], 3);
  T (&a2_4[i].a4[0], &a2_4[j].a4[2], 2);
  T (&a2_4[i].a4[0], &a2_4[j].a4[3], 1);
}

struct { int i; } a2[2][8];

void test_single_2_dim_major (int i)
{
  memcpy (&a2[i], &a2[0], sizeof *a2);   /* { dg-bogus "\\\[-Wrestrict]" } */
}

void test_single_2_dim_minor (int i)
{
  memcpy (&a2[i][0], &a2[0][0], sizeof a2[0][0]);   /* { dg-bogus "\\\[-Wrestrict]" } */
}

void test_single_2_dim_major_minor (int i, int j)
{
  memcpy (&a2[i][j], &a2[0][0], sizeof a2[0][0]);   /* { dg-bogus "\\\[-Wrestrict]" } */
}
