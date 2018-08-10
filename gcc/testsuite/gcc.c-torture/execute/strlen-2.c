/* PR tree-optimization/86532 - Wrong code due to a wrong strlen folding  */

extern __SIZE_TYPE__ strlen (const char*);

static const char a[2][3] = { "1", "12" };
static const char b[2][2][5] = { { "1", "12" }, { "123", "1234" } };

volatile int v0 = 0;
volatile int v1 = 1;
volatile int v2 = 2;

#define A(expr)								\
  ((expr) ? (void)0 : (__builtin_printf ("assertion on line %i: %s\n",	\
					 __LINE__, #expr),		\
		       __builtin_abort ()))

void test_array_ref_2_3 (void)
{
  A (strlen (a[v0]) == 1);
  A (strlen (&a[v0][v0]) == 1);
  A (strlen (&a[0][v0]) == 1);
  A (strlen (&a[v0][0]) == 1);

  A (strlen (a[v1]) == 2);
  A (strlen (&a[v1][0]) == 2);
  A (strlen (&a[1][v0]) == 2);
  A (strlen (&a[v1][v0]) == 2);

  A (strlen (&a[v1][1]) == 1);
  A (strlen (&a[v1][1]) == 1);

  A (strlen (&a[v1][2]) == 0);
  A (strlen (&a[v1][v2]) == 0);

  int i0 = 0;
  int i1 = i0 + 1;
  int i2 = i1 + 1;

  A (strlen (a[v0]) == 1);
  A (strlen (&a[v0][v0]) == 1);
  A (strlen (&a[i0][v0]) == 1);
  A (strlen (&a[v0][i0]) == 1);

  A (strlen (a[v1]) == 2);
  A (strlen (&a[v1][i0]) == 2);
  A (strlen (&a[i1][v0]) == 2);
  A (strlen (&a[v1][v0]) == 2);

  A (strlen (&a[v1][i1]) == 1);
  A (strlen (&a[v1][i1]) == 1);

  A (strlen (&a[v1][i2]) == 0);
  A (strlen (&a[v1][v2]) == 0);
}

void test_array_off_2_3 (void)
{
  A (strlen (a[0] + 0) == 1);
  A (strlen (a[0] + v0) == 1);
  A (strlen (a[v0] + 0) == 1);
  A (strlen (a[v0] + v0) == 1);

  A (strlen (a[v1] + 0) == 2);
  A (strlen (a[1] + v0) == 2);
  A (strlen (a[v1] + 0) == 2);
  A (strlen (a[v1] + v0) == 2);

  A (strlen (a[v1] + 1) == 1);
  A (strlen (a[v1] + v1) == 1);

  A (strlen (a[v1] + 2) == 0);
  A (strlen (a[v1] + v2) == 0);

  int i0 = 0;
  int i1 = i0 + 1;
  int i2 = i1 + 1;

  A (strlen (a[i0] + i0) == 1);
  A (strlen (a[i0] + v0) == 1);
  A (strlen (a[v0] + i0) == 1);
  A (strlen (a[v0] + v0) == 1);

  A (strlen (a[v1] + i0) == 2);
  A (strlen (a[i1] + v0) == 2);
  A (strlen (a[v1] + i0) == 2);
  A (strlen (a[v1] + v0) == 2);

  A (strlen (a[v1] + i1) == 1);
  A (strlen (a[v1] + v1) == 1);

  A (strlen (a[v1] + i2) == 0);
  A (strlen (a[v1] + v2) == 0);
}

void test_array_ref_2_2_5 (void)
{
  A (strlen (b[0][v0]) == 1);
  A (strlen (b[v0][0]) == 1);

  A (strlen (&b[0][0][v0]) == 1);
  A (strlen (&b[0][v0][0]) == 1);
  A (strlen (&b[v0][0][0]) == 1);

  A (strlen (&b[0][v0][v0]) == 1);
  A (strlen (&b[v0][0][v0]) == 1);
  A (strlen (&b[v0][v0][0]) == 1);

  A (strlen (b[0][v1]) == 2);
  A (strlen (b[v1][0]) == 3);

  A (strlen (&b[0][0][v1]) == 0);
  A (strlen (&b[0][v1][0]) == 2);
  A (strlen (&b[v0][0][0]) == 1);

  A (strlen (&b[0][v0][v0]) == 1);
  A (strlen (&b[v0][0][v0]) == 1);
  A (strlen (&b[v0][v0][0]) == 1);

  A (strlen (&b[0][v1][v1]) == 1);
  A (strlen (&b[v1][0][v1]) == 2);
  A (strlen (&b[v1][v1][0]) == 4);
  A (strlen (&b[v1][v1][1]) == 3);
  A (strlen (&b[v1][v1][2]) == 2);

  int i0 = 0;
  int i1 = i0 + 1;
  int i2 = i1 + 1;

  A (strlen (b[i0][v0]) == 1);
  A (strlen (b[v0][i0]) == 1);

  A (strlen (&b[i0][i0][v0]) == 1);
  A (strlen (&b[i0][v0][i0]) == 1);
  A (strlen (&b[v0][i0][i0]) == 1);

  A (strlen (&b[i0][v0][v0]) == 1);
  A (strlen (&b[v0][i0][v0]) == 1);
  A (strlen (&b[v0][v0][i0]) == 1);

  A (strlen (b[i0][v1]) == 2);
  A (strlen (b[v1][i0]) == 3);

  A (strlen (&b[i0][i0][v1]) == 0);
  A (strlen (&b[i0][v1][i0]) == 2);
  A (strlen (&b[v0][i0][i0]) == 1);

  A (strlen (&b[i0][v0][v0]) == 1);
  A (strlen (&b[v0][i0][v0]) == 1);
  A (strlen (&b[v0][v0][i0]) == 1);

  A (strlen (&b[i0][v1][v1]) == 1);
  A (strlen (&b[v1][i0][v1]) == 2);
  A (strlen (&b[v1][v1][i0]) == 4);
  A (strlen (&b[v1][v1][i1]) == 3);
  A (strlen (&b[v1][v1][i2]) == 2);
}

void test_array_off_2_2_5 (void)
{
  A (strlen (b[0][0] + v0) == 1);
  A (strlen (b[0][v0] + v0) == 1);
  A (strlen (b[v0][0] + v0) == 1);
  A (strlen (b[v0][v0] + v0) == 1);

  A (strlen (b[0][0] + v1) == 0);
  A (strlen (b[0][v1] + 0) == 2);
  A (strlen (b[v0][0] + 0) == 1);

  A (strlen (b[0][v0] + v0) == 1);
  A (strlen (b[v0][0] + v0) == 1);
  A (strlen (b[v0][v0] + 0) == 1);

  A (strlen (b[0][v1] + v1) == 1);
  A (strlen (b[v1][0] + v1) == 2);
  A (strlen (b[v1][v1] + 0) == 4);
  A (strlen (b[v1][v1] + 1) == 3);
  A (strlen (b[v1][v1] + 2) == 2);

  int i0 = 0;
  int i1 = i0 + 1;
  int i2 = i1 + 1;

  A (strlen (b[i0][i0] + v0) == 1);
  A (strlen (b[i0][v0] + v0) == 1);
  A (strlen (b[v0][i0] + v0) == 1);
  A (strlen (b[v0][v0] + v0) == 1);

  A (strlen (b[i0][i0] + v1) == 0);
  A (strlen (b[i0][v1] + i0) == 2);
  A (strlen (b[v0][i0] + i0) == 1);

  A (strlen (b[i0][v0] + v0) == 1);
  A (strlen (b[v0][i0] + v0) == 1);
  A (strlen (b[v0][v0] + i0) == 1);

  A (strlen (b[i0][v1] + v1) == 1);
  A (strlen (b[v1][i0] + v1) == 2);
  A (strlen (b[v1][v1] + i0) == 4);
  A (strlen (b[v1][v1] + i1) == 3);
  A (strlen (b[v1][v1] + i2) == 2);
}

int main ()
{
  test_array_ref_2_3 ();
  test_array_off_2_3 ();

  test_array_ref_2_2_5 ();
  test_array_off_2_2_5 ();
}
