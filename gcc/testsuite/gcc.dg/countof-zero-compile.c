/* { dg-do compile } */
/* { dg-options "-std=gnu2y" } */

static int z[0];
static int y[_Countof (z)];

_Static_assert(_Countof (y) == 0);

void
completed (void)
{
  int z[] = {};

  static_assert (_Countof (z) == 0);
}

void zro_fix (int i,
	      char (*a)[0][5],
	      int (*x)[_Countof (*a)],
	      short (*)[_Generic(x, int (*)[0]: 1)]);
void zro_var (int i,
	      char (*a)[0][i], /* dg-warn "variable" */
	      int (*x)[_Countof (*a)],
	      short (*)[_Generic(x, int (*)[0]: 1)]);
void zro_uns (int i,
	      char (*a)[0][*],
	      int (*x)[_Countof (*a)],
	      short (*)[_Generic(x, int (*)[0]: 1)]);

void
const_expr(void)
{
  int n = 7;

  _Static_assert (_Countof (int [0][3]) == 0);
  _Static_assert (_Countof (int [0]) == 0);
  _Static_assert (_Countof (int [0][n]) == 0);
}
