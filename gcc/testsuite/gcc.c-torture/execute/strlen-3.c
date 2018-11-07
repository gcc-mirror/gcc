/* PR tree-optimization/86532 - Wrong code due to a wrong strlen folding
   starting with r262522
   Exercise strlen() with a multi-dimensional array of strings with
   embedded nuls.  */

extern __SIZE_TYPE__ strlen (const char*);

static const char a[2][3][9] = {
  { "1", "1\0002" },
  { "12\0003", "123\0004" }
};

volatile int v0 = 0;
volatile int v1 = 1;
volatile int v2 = 2;
volatile int v3 = 3;
volatile int v4 = 4;
volatile int v5 = 5;
volatile int v6 = 6;
volatile int v7 = 7;

#define A(expr)								\
  ((expr) ? (void)0 : (__builtin_printf ("assertion on line %i: %s\n",	\
					 __LINE__, #expr),		\
		       __builtin_abort ()))

void test_array_ref (void)
{
  int i0 = 0;
  int i1 = i0 + 1;
  int i2 = i1 + 1;
  int i3 = i2 + 1;
  int i4 = i3 + 1;
  int i5 = i4 + 1;
  int i6 = i5 + 1;
  int i7 = i6 + 1;

  A (strlen (a[0][0]) == 1);
  A (strlen (a[0][1]) == 1);

  A (strlen (a[1][0]) == 2);
  A (strlen (a[1][1]) == 3);

  A (strlen (&a[0][0][0]) == 1);
  A (strlen (&a[0][1][0]) == 1);

  A (strlen (&a[1][0][0]) == 2);
  A (strlen (&a[1][1][0]) == 3);

  A (strlen (&a[0][0][0] + 1) == 0);
  A (strlen (&a[0][1][0] + 1) == 0);
  A (strlen (&a[0][1][0] + 2) == 1);
  A (strlen (&a[0][1][0] + 3) == 0);
  A (strlen (&a[0][1][0] + 7) == 0);

  A (strlen (&a[1][0][0] + 1) == 1);
  A (strlen (&a[1][1][0] + 1) == 2);
  A (strlen (&a[1][1][0] + 2) == 1);
  A (strlen (&a[1][1][0] + 7) == 0);


  A (strlen (a[i0][i0]) == 1);
  A (strlen (a[i0][i1]) == 1);

  A (strlen (a[i1][i0]) == 2);
  A (strlen (a[i1][i1]) == 3);

  A (strlen (&a[i0][i0][i0]) == 1);
  A (strlen (&a[i0][i1][i0]) == 1);
  A (strlen (&a[i0][i1][i1]) == 0);
  A (strlen (&a[i0][i1][i2]) == 1);
  A (strlen (&a[i0][i1][i3]) == 0);
  A (strlen (&a[i0][i1][i3]) == 0);

  A (strlen (&a[i1][i0][i0]) == 2);
  A (strlen (&a[i1][i1][i0]) == 3);
  A (strlen (&a[i1][i1][i1]) == 2);
  A (strlen (&a[i1][i1][i2]) == 1);
  A (strlen (&a[i1][i1][i3]) == 0);
  A (strlen (&a[i1][i1][i4]) == 1);
  A (strlen (&a[i1][i1][i5]) == 0);
  A (strlen (&a[i1][i1][i6]) == 0);
  A (strlen (&a[i1][i1][i7]) == 0);

  A (strlen (&a[i0][i0][i0] + i1) == 0);
  A (strlen (&a[i0][i1][i0] + i1) == 0);
  A (strlen (&a[i0][i1][i0] + i7) == 0);

  A (strlen (&a[i1][i0][i0] + i1) == 1);
  A (strlen (&a[i1][i1][i0] + i1) == 2);
  A (strlen (&a[i1][i1][i0] + i2) == 1);
  A (strlen (&a[i1][i1][i0] + i3) == 0);
  A (strlen (&a[i1][i1][i0] + i4) == 1);
  A (strlen (&a[i1][i1][i0] + i5) == 0);
  A (strlen (&a[i1][i1][i0] + i6) == 0);
  A (strlen (&a[i1][i1][i0] + i7) == 0);


  A (strlen (a[i0][i0]) == 1);
  A (strlen (a[i0][i1]) == 1);

  A (strlen (a[i1][i0]) == 2);
  A (strlen (a[i1][i1]) == 3);

  A (strlen (&a[i0][i0][i0]) == 1);
  A (strlen (&a[i0][i1][i0]) == 1);

  A (strlen (&a[i1][i0][i0]) == 2);
  A (strlen (&a[i1][i1][i0]) == 3);

  A (strlen (&a[i0][i0][i0] + v1) == 0);
  A (strlen (&a[i0][i0][i0] + v2) == 0);
  A (strlen (&a[i0][i0][i0] + v7) == 0);

  A (strlen (&a[i0][i1][i0] + v1) == 0);
  A (strlen (&a[i0][i1][i0] + v2) == 1);
  A (strlen (&a[i0][i1][i0] + v3) == 0);

  A (strlen (&a[i1][i0][i0] + v1) == 1);
  A (strlen (&a[i1][i1][i0] + v1) == 2);
  A (strlen (&a[i1][i1][i0] + v2) == 1);
  A (strlen (&a[i1][i1][i0] + v3) == 0);
  A (strlen (&a[i1][i1][i0] + v4) == 1);
  A (strlen (&a[i1][i1][i0] + v5) == 0);
  A (strlen (&a[i1][i1][i0] + v6) == 0);
  A (strlen (&a[i1][i1][i0] + v7) == 0);
}

int main (void)
{
  test_array_ref ();
}
