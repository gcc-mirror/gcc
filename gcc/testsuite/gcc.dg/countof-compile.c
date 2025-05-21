/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

#define NULL  ((void *) 0)

extern int x[];

static int w[] = {1, 2, 3};

void
completed (void)
{
  int i = 42;
  int a[] = {1, 2, i};

  _Static_assert(_Countof (w) == 3);
  _Static_assert(_Countof (a) == 3);
}

void
incomplete (int p[])
{
  _Countof (x);  /* { dg-error "incomplete" } */

  /* We want to support array parameters in the future,
     which should change this from "invalid" to "incomplete".  */
  _Countof (p);  /* { dg-error "invalid" } */
}

void
fam (void)
{
  struct {
    int x;
    int fam[];
  } s;

  _Countof (s.fam); /* { dg-error "incomplete" } */
}

void
param (int n, int p[n])
{
  /* We want to support array parameters in the future,
     which would make this work.  */
  _Countof (p);  /* { dg-error "invalid" } */
}

void fix_fix (int i, char (*a)[3][5], int (*x)[_Countof (*a)],
	      short (*)[_Generic(x, int (*)[3]: 1)]);
void fix_var (int i, char (*a)[3][i], int (*x)[_Countof (*a)],
	      short (*)[_Generic(x, int (*)[3]: 1)]);
void fix_uns (int i, char (*a)[3][*], int (*x)[_Countof (*a)],
	      short (*)[_Generic(x, int (*)[3]: 1)]);

void
func (void)
{
  int  i3[3];
  int  i5[5];
  char c35[3][5];

  fix_fix (5, &c35, &i3, NULL);
  fix_fix (5, &c35, &i5, NULL); /* { dg-error "incompatible-pointer-types" } */

  fix_var (5, &c35, &i3, NULL);
  fix_var (5, &c35, &i5, NULL); /* { dg-error "incompatible-pointer-types" } */

  fix_uns (5, &c35, &i3, NULL);
  fix_uns (5, &c35, &i5, NULL); /* { dg-error "incompatible-pointer-types" } */
}

void
non_arr(void)
{
  int x;
  int *p;
  struct s {
    int x[3];
  } s;

  _Countof (x); /* { dg-error "invalid" } */
  _Countof (int); /* { dg-error "invalid" } */
  _Countof (s); /* { dg-error "invalid" } */
  _Countof (struct s); /* { dg-error "invalid" } */
  _Countof (&x); /* { dg-error "invalid" } */
  _Countof (p); /* { dg-error "invalid" } */
  _Countof (int *); /* { dg-error "invalid" } */
  _Countof (&s.x); /* { dg-error "invalid" } */
  _Countof (int (*)[3]); /* { dg-error "invalid" } */
}

static int f1();
static int f2(); /* { dg-error "never defined" } */
int a[10][9];
int n;

void
syms(void)
{
  int b[n][n];

  _Countof (a[f1()]);
  _Countof (b[f2()]);
}

void
no_parens(void)
{
  _Static_assert(_Countof a == 10);
  _Static_assert(_Countof *a == 9);
  _Static_assert(_Countof (int [3]) {} == 3);

  _Countof int [3]; /* { dg-error "expected expression before" } */
}

void
const_expr(void)
{
  int n = 7;

  _Static_assert (_Countof (int [3][n]) == 3);
  _Static_assert (_Countof (int [n][3]) == 7); /* { dg-error "not constant" } */
}
