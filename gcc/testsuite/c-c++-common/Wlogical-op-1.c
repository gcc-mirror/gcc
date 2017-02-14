/* PR c/63357 */
/* { dg-do compile } */
/* { dg-options "-Wlogical-op" } */

#ifndef __cplusplus
# define bool _Bool
# define true 1
# define false 0
#endif

#if __SIZEOF_INT__ < 4
  __extension__ typedef __INT32_TYPE__ int32_t;
  __extension__ typedef __UINT32_TYPE__ uint32_t;
  __extension__ typedef __INT16_TYPE__ int16_t;
#else
  typedef int int32_t;
  typedef unsigned int uint32_t;
  typedef short int16_t;
#endif

extern int32_t bar (void);
extern int32_t *p;
struct R { int32_t a, b; } S;

void
andfn (int32_t a, int32_t b)
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
  if ((uint32_t) a && a) {}	/* { dg-warning "logical .and. of equal expressions" } */

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
  if (a && (int16_t) a) {}
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
orfn (int32_t a, int32_t b)
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
  if ((uint32_t) a || a) {}	/* { dg-warning "logical .or. of equal expressions" } */

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
  if (a || (int16_t) a) {}
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
