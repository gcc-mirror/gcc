/* PR tree-optimization/86622 - incorrect strlen of array of array plus
   variable offset
   Exercise strlen() with a multi-dimensional array of strings with
   offsets.  */

extern int printf (const char*, ...);
extern __SIZE_TYPE__ strlen (const char*);

typedef char A28[28];
typedef A28 A3_28[3];
typedef A3_28 A2_3_28[2];

static const A2_3_28 a = {
  /* [0][0]    [0][1]         [0][2] */
  { "1\00012", "123\0001234", "12345\000123456" },
  /* [1][0]    [1][1]         [1][2] */
  { "1234567\00012345678", "123456789\0001234567890", "12345678901\000123456789012" }
};

volatile int v0 = 0;
volatile int v1 = 1;
volatile int v2 = 2;
volatile int v3 = 3;
volatile int v4 = 4;
volatile int v5 = 5;
volatile int v6 = 6;
volatile int v7 = 7;

#define A(expr, N)							\
  ((strlen (expr) == N)							\
   ? (void)0 : (printf ("line %i: strlen (%s = \"%s\") != %i\n",	\
			__LINE__, #expr, expr, N),			\
		__builtin_abort ()))

/* Verify that strlen() involving pointer to array arguments computes
   the correct result.  */

void test_array_ptr (void)
{
  /* Compute the length of the string at the refeenced array.  */
  A (*(&a[0][0] + 0), 1);
  A (*(&a[0][0] + 1), 3);
  A (*(&a[0][0] + 2), 5);

  A (*(&a[0][1] - 1), 1);
  A (*(&a[0][1] + 0), 3);
  A (*(&a[0][1] + 1), 5);

  A (*(&a[0][2] - 2), 1);
  A (*(&a[0][2] - 1), 3);
  A (*(&a[0][2] + 0), 5);

  A (*(&a[1][0] + 0), 7);
  A (*(&a[1][0] + 1), 9);
  A (*(&a[1][0] + 2), 11);

  A (*(&a[1][1] - 1), 7);
  A (*(&a[1][1] + 0), 9);
  A (*(&a[1][1] + 1), 11);

  A (*(&a[1][2] - 2), 7);
  A (*(&a[1][2] - 1), 9);
  A (*(&a[1][2] - 0), 11);

  /* Compute the length of the string past the first nul.  */
  A (*(&a[0][0] + 0) + 2, 2);
  A (*(&a[0][0] + 1) + 4, 4);
  A (*(&a[0][0] + 2) + 6, 6);

  /* Compute the length of the string past the second nul.  */
  A (*(&a[0][0] + 0) + 5, 0);
  A (*(&a[0][0] + 1) + 10, 0);
  A (*(&a[0][0] + 2) + 14, 0);

  int i0 = 0;
  int i1 = i0 + 1;
  int i2 = i1 + 1;
  int i3 = i2 + 1;
  int i4 = i3 + 1;
  int i5 = i4 + 1;

  A (*(&a[0][0] + i0), 1);
  A (*(&a[0][0] + i1), 3);
  A (*(&a[0][0] + i2), 5);

  A (*(&a[0][1] - i1), 1);
  A (*(&a[0][1] + i0), 3);
  A (*(&a[0][1] + i1), 5);

  A (*(&a[0][2] - i2), 1);
  A (*(&a[0][2] - i1), 3);
  A (*(&a[0][2] + i0), 5);

  A (*(&a[1][0] + i0), 7);
  A (*(&a[1][0] + i1), 9);
  A (*(&a[1][0] + i2), 11);

  A (*(&a[1][1] - i1), 7);
  A (*(&a[1][1] + i0), 9);
  A (*(&a[1][1] + i1), 11);

  A (*(&a[1][2] - i2), 7);
  A (*(&a[1][2] - i1), 9);
  A (*(&a[1][2] - i0), 11);


  A (*(&a[i0][i0] + i0), 1);
  A (*(&a[i0][i0] + i1), 3);
  A (*(&a[i0][i0] + i2), 5);

  A (*(&a[i0][i1] - i1), 1);
  A (*(&a[i0][i1] + i0), 3);
  A (*(&a[i0][i1] + i1), 5);

  A (*(&a[i0][i2] - i2), 1);
  A (*(&a[i0][i2] - i1), 3);
  A (*(&a[i0][i2] + i0), 5);

  A (*(&a[i1][i0] + i0), 7);
  A (*(&a[i1][i0] + i1), 9);
  A (*(&a[i1][i0] + i2), 11);

  A (*(&a[i1][i1] - i1), 7);
  A (*(&a[i1][i1] + i0), 9);
  A (*(&a[i1][i1] + i1), 11);

  A (*(&a[i1][i2] - i2), 7);
  A (*(&a[i1][i2] - i1), 9);
  A (*(&a[i1][i2] - i0), 11);


  A (*(&a[i0][i0] + v0), 1);
  A (*(&a[i0][i0] + v1), 3);
  A (*(&a[i0][i0] + v2), 5);

  A (*(&a[i0][i1] - v1), 1);
  A (*(&a[i0][i1] + v0), 3);
  A (*(&a[i0][i1] + v1), 5);

  A (*(&a[i0][i2] - v2), 1);
  A (*(&a[i0][i2] - v1), 3);
  A (*(&a[i0][i2] + v0), 5);

  A (*(&a[i1][i0] + v0), 7);
  A (*(&a[i1][i0] + v1), 9);
  A (*(&a[i1][i0] + v2), 11);

  A (*(&a[i1][i1] - v1), 7);
  A (*(&a[i1][i1] + v0), 9);
  A (*(&a[i1][i1] + v1), 11);

  A (*(&a[i1][i2] - v2), 7);
  A (*(&a[i1][i2] - v1), 9);
  A (*(&a[i1][i2] - v0), 11);


  A (*(&a[i0][i0] + v0) + i1, 0);
  A (*(&a[i0][i0] + v1) + i2, 1);
  A (*(&a[i0][i0] + v2) + i3, 2);

  A (*(&a[i0][i1] - v1) + v1, 0);
  A (*(&a[i0][i1] + v0) + v3, 0);
  A (*(&a[i0][i1] + v1) + v5, 0);

  A (*(&a[i0][v1] - i1) + i1, 0);
  A (*(&a[i0][v1] + i0) + i3, 0);
  A (*(&a[i0][v1] + i1) + i5, 0);
}

static const A3_28* const pa0 = &a[0];
static const A3_28* const pa1 = &a[1];

static const A3_28* const paa[] = { &a[0], &a[1] };

/* Verify that strlen() involving pointers and arrays of pointers
   to array arguments computes the correct result.  */

void test_ptr_array (void)
{
  int i0 = 0;
  int i1 = i0 + 1;
  int i2 = i1 + 1;
  int i3 = i2 + 1;

  A (*((*pa0) + i0), 1);
  A (*((*pa0) + i1), 3);
  A (*((*pa0) + i2), 5);

  A (*(pa0[0] + i0), 1);
  A (*(pa0[0] + i1), 3);
  A (*(pa0[0] + i2), 5);

  A ((*pa0)[i0] + i1, 0);
  A ((*pa0)[i1] + i2, 1);
  A ((*pa0)[i2] + i3, 2);


  A (*((*pa1) + i0), 7);
  A (*((*pa1) + i1), 9);
  A (*((*pa1) + i2), 11);

  A (*(pa1[0] + i0), 7);
  A (*(pa1[0] + i1), 9);
  A (*(pa1[0] + i2), 11);

  A ((*pa1)[i0] + i1, 6);
  A ((*pa1)[i1] + i2, 7);
  A ((*pa1)[i2] + i3, 8);

  A (*(*(paa[0]) + i0), 1);
  A (*(*(paa[0]) + i1), 3);
  A (*(*(paa[0]) + i2), 5);

  A (*(*(paa[1]) + i0), 7);
  A (*(*(paa[1]) + i1), 9);
  A (*(*(paa[1]) + i2), 11);

  A (*(*(paa[1]) - i1), 5);
  A (*(*(paa[1]) - i2), 3);
  A (*(*(paa[1]) - i3), 1);

  A (*(*(paa[0]) + i0) + i1, 0);
  A (*(*(paa[0]) + i1) + i2, 1);
  A (*(*(paa[0]) + i2) + i3, 2);
}

int main (void)
{
  test_array_ptr ();

  test_ptr_array ();
}
