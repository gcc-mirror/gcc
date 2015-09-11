/* PR c/63357 */
/* { dg-do compile } */
/* { dg-options "-Wlogical-op" } */

#ifndef __cplusplus
# define bool _Bool
# define true 1
# define false 0
#endif

extern int bar (void);
extern int *p;
struct R { int a, b; } S;

void
andfn (int a, int b)
{
  if (a && a) {}		/* { dg-warning "logical .and. of equal expressions" } */
  if (!a && !a) {}		/* { dg-warning "logical .and. of equal expressions" } */
  if (!!a && !!a) {}		/* { dg-warning "logical .and. of equal expressions" } */
  if (a > 0 && a > 0) {}	/* { dg-warning "logical .and. of equal expressions" } */
  if (a < 0 && a < 0) {}	/* { dg-warning "logical .and. of equal expressions" } */
  if (a == 0 && a == 0) {}	/* { dg-warning "logical .and. of equal expressions" } */
  if (a <= 0 && a <= 0) {}	/* { dg-warning "logical .and. of equal expressions" } */
  if (a >= 0 && a >= 0) {}	/* { dg-warning "logical .and. of equal expressions" } */
  if (a == 0 && !(a != 0)) {}	/* { dg-warning "logical .and. of equal expressions" } */

  if (a && a && a) {}		/* { dg-warning "logical .and. of equal expressions" } */
  if ((a + 1) && (a + 1)) {}	/* { dg-warning "logical .and. of equal expressions" } */
  if ((10 * a) && (a * 10)) {}	/* { dg-warning "logical .and. of equal expressions" } */
  if (!!a && a) {}		/* { dg-warning "logical .and. of equal expressions" } */

  if (*p && *p) {}		/* { dg-warning "logical .and. of equal expressions" } */
  if (p[0] && p[0]) {}		/* { dg-warning "logical .and. of equal expressions" } */
  if (S.a && S.a) {}		/* { dg-warning "logical .and. of equal expressions" } */
  if ((bool) a && (bool) a) {}	/* { dg-warning "logical .and. of equal expressions" } */
  if ((unsigned) a && a) {}	/* { dg-warning "logical .and. of equal expressions" } */

  /* Stay quiet here.  */
  if (a && b) {}
  if (!a && !b) {}
  if (!!a && !!b) {}
  if (a > 0 && b > 0) {}
  if (a < 0 && b < 0) {}
  if (a == 0 && b == 0) {}
  if (a <= 0 && b <= 0) {}
  if (a >= 0 && b >= 0) {}

  if (a > 0 && a > 1) {}
  if (a > -2 && a > 1) {}
  if (a && (short) a) {}
  if ((char) a && a) {}
  if (++a && a) {}
  if (++a && ++a) {}
  if (a && --a) {}
  if (a && a / 2) {}
  if (bar () && bar ()) {}
  if (p && *p) {}
  if (p[0] && p[1]) {}
  if (S.a && S.b) {}
}

void
orfn (int a, int b)
{
  if (a || a) {}		/* { dg-warning "logical .or. of equal expressions" } */
  if (!a || !a) {}		/* { dg-warning "logical .or. of equal expressions" } */
  if (!!a || !!a) {}		/* { dg-warning "logical .or. of equal expressions" } */
  if (a > 0 || a > 0) {}	/* { dg-warning "logical .or. of equal expressions" } */
  if (a < 0 || a < 0) {}	/* { dg-warning "logical .or. of equal expressions" } */
  if (a == 0 || a == 0) {}	/* { dg-warning "logical .or. of equal expressions" } */
  if (a <= 0 || a <= 0) {}	/* { dg-warning "logical .or. of equal expressions" } */
  if (a >= 0 || a >= 0) {}	/* { dg-warning "logical .or. of equal expressions" } */
  if (a == 0 || !(a != 0)) {}	/* { dg-warning "logical .or. of equal expressions" } */

  if (a || a || a) {}		/* { dg-warning "logical .or. of equal expressions" } */
  if ((a + 1) || (a + 1)) {}	/* { dg-warning "logical .or. of equal expressions" } */
  if ((10 * a) || (a * 10)) {}	/* { dg-warning "logical .or. of equal expressions" } */
  if (!!a || a) {}		/* { dg-warning "logical .or. of equal expressions" } */

  if (*p || *p) {}		/* { dg-warning "logical .or. of equal expressions" } */
  if (p[0] || p[0]) {}		/* { dg-warning "logical .or. of equal expressions" } */
  if (S.a || S.a) {}		/* { dg-warning "logical .or. of equal expressions" } */
  if ((bool) a || (bool) a) {}	/* { dg-warning "logical .or. of equal expressions" } */
  if ((unsigned) a || a) {}	/* { dg-warning "logical .or. of equal expressions" } */

  /* Stay quiet here.  */
  if (a || b) {}
  if (!a || !b) {}
  if (!!a || !!b) {}
  if (a > 0 || b > 0) {}
  if (a < 0 || b < 0) {}
  if (a == 0 || b == 0) {}
  if (a <= 0 || b <= 0) {}
  if (a >= 0 || b >= 0) {}

  if (a > 0 || a > 1) {}
  if (a > -2 || a > 1) {}
  if (a || (short) a) {}
  if ((char) a || a) {}
  if (++a || a) {}
  if (++a || ++a) {}
  if (a || --a) {}
  if (a || a / 2) {}
  if (bar () || bar ()) {}
  if (p || *p) {}
  if (p[0] || p[1]) {}
  if (S.a || S.b) {}
}
