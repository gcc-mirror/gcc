#include "harness.h"
#include <stdarg.h>
#include <stddef.h>
#include <string.h>

typedef struct n_a
{
  signed char m1;
  short m2;
  int m3;
  double m4;
  vector float m5;
}
n_a;

static n_a gn_a;

static int
lay(char *p, int start, int end, int n)
{
  int b;
  unsigned char ch;
  unsigned int mask;

  start *= 8;
  end *= 8;
  n *= 8;

  for (b = 0; b + 8 <= start; b += 8)
    {
      ch = *p++;
      if (ch != 0xff)
	for (mask = 0x80; mask; b++, mask >>= 1)
	  if ((ch & mask) != mask)
	    return b;
    }

  if (b < start)
    {
      ch = *p++;
      for (mask = 0x80; b < start; b++, mask >>= 1)
	if ((ch & mask) != mask)
	  return b;
      for (; mask && b < end; b++, mask >>= 1)
	if ((ch & mask) != 0)
	  return b;
    }

  for (; b + 8 <= end; b += 8)
    {
      ch = *p++;
      if (ch != 0)
	for (mask = 0x80; mask; b++, mask >>= 1)
	  if ((ch & mask) != 0)
	    return b;
    }

  if (b < end)
    {
      ch = *p++;
      for (mask = 0x80; b < end; b++, mask >>= 1)
	if ((ch & mask) != 0)
	  return b;
      for (; mask && b < n; b++, mask >>= 1)
	if ((ch & mask) != mask)
	  return b;
    }

  for (; b + 8 <= n; b += 8)
    {
      ch = *p++;
      if (ch != 0xff)
	for (mask = 0x80; mask; b++, mask >>= 1)
	  if ((ch & mask) != mask)
	    return b;
    }

  return n;
}

static void
initn_a(signed char p1, short p2, int p3, double p4, vector float p5)
{
  n_a i;

  i.m1 = p1;
  i.m2 = p2;
  i.m3 = p3;
  i.m4 = p4;
  i.m5 = p5;

  check(i.m1 == 77, "i.m1");
  check(i.m2 == 1924, "i.m2");
  check(i.m3 == -1471601920, "i.m3");
  check(vec_all_eq(i.m5, ((vector float){-1.38e+09, 5.96e+08, 6.88e+08, -3.2e+08})),
	"i.m5");

  check(sizeof(n_a) == 32, "sizeof(n_a)");

  check(offsetof(n_a, m1) == 0,  "offsetof(m1)");
  check(offsetof(n_a, m2) == 2,  "offsetof(m2)");
  check(offsetof(n_a, m3) == 4,  "offsetof(m3)");
  check(offsetof(n_a, m4) == 8,  "offsetof(m4)");
  check(offsetof(n_a, m5) == 16, "offsetof(m5)");

  check(sizeof(i.m1) == 1,  "sizeof(m1)");
  check(sizeof(i.m2) == 2,  "sizeof(m2)");
  check(sizeof(i.m3) == 4,  "sizeof(m3)");
  check(sizeof(i.m4) == 8,  "sizeof(m4)");
  check(sizeof(i.m5) == 16, "sizeof(m5)");

#define lay_check(field) do {				\
  memset((char *)&i, 0xFF, sizeof(i));			\
  lay_reset(field);					\
  check(lay((char *)&i,					\
	    offsetof(n_a, field),			\
	    offsetof(n_a, field) + sizeof(i.field),	\
	    sizeof(i)) == sizeof(i)*8,			\
	"lay(" #field ")");				\
  } while (0)
#define lay_reset(field) i.field = 0

  lay_check(m1);
  lay_check(m2);
  lay_check(m3);
  lay_check(m4);
#undef lay_reset
#define lay_reset(field) i.field = ((vector float){0,0,0,0})
  lay_check(m5);

#undef lay_check
#undef lay_reset
}

n_a
valuen_a(void)
{
  return gn_a;
}

n_a *
addrn_a(void)
{
  return &gn_a;
}

static void
eqn_a(n_a * a)
{
  check(a->m1 == 77, "a->m1");
  check(a->m2 == 1924, "a->m2");
  check(a->m3 == -1471601920, "a->m3");
  check(vec_all_eq(a->m5, ((vector float){-1.38e+09, 5.96e+08, 6.88e+08, -3.2e+08})),
	"a->m5");
}

static void
getsn_a(n_a * a)
{
  a->m1 = 77;
  a->m2 = 1924;
  a->m3 = -1471601920;
  a->m4 = 3.65e+18;
  a->m5 = ((vector float){-1.38e+09, 5.96e+08, 6.88e+08, -3.2e+08});
}

static void
varlistn_a(signed char p1, va_list ap)
{
  n_a q;
  q.m1 = p1;
  q.m2 = va_arg(ap, int);
  q.m3 = va_arg(ap, int);
  q.m4 = va_arg(ap, double);
  q.m5 = va_arg(ap, vector float);

  check(q.m1 == 77, "q.m1");
  check(q.m2 == 1924, "q.m2");
  check(q.m3 == -1471601920, "q.m3");
  check(vec_all_eq(q.m5, ((vector float){-1.38e+09, 5.96e+08, 6.88e+08, -3.2e+08})),
	"q.m5");
}

static void
varargsn_a(signed char p1, ...)
{
  n_a q, r;
  va_list ap;

  va_start(ap, p1);
  q.m1 = p1;
  q.m2 = va_arg(ap, int);
  q.m3 = va_arg(ap, int);
  q.m4 = va_arg(ap, double);
  q.m5 = va_arg(ap, vector float);
  va_end(ap);

  check(q.m1 == 77, "q.m1");
  check(q.m2 == 1924, "q.m2");
  check(q.m3 == -1471601920, "q.m3");
  check(vec_all_eq(q.m5, ((vector float){-1.38e+09, 5.96e+08, 6.88e+08, -3.2e+08})),
	"q.m5");

  va_start(ap, p1);
  r.m1 = p1;
  r.m2 = va_arg(ap, int);
  r.m3 = va_arg(ap, int);
  r.m4 = va_arg(ap, double);
  r.m5 = va_arg(ap, vector float);
  va_end(ap);

  check(r.m1 == 77, "r.m1");
  check(r.m2 == 1924, "r.m2");
  check(r.m3 == -1471601920, "r.m3");
  check(vec_all_eq(r.m5, ((vector float){-1.38e+09, 5.96e+08, 6.88e+08, -3.2e+08})),
	"r.m5");

  va_start(ap, p1);
  varlistn_a(p1, ap);
  va_end(ap);
}

static void
test()
{
  static struct
  {
    char a;
    n_a b;
  }
  s;
  n_a v[3], a, *p;

  static n_a i = { 77, 1924, -1471601920, 3.65e+18, {-1.38e+09, 5.96e+08, 6.88e+08, -3.2e+08} };

  memset((char *)&(v), -1, sizeof(v));
  v[1] = s.b;
  check(lay((char *)&v, sizeof(n_a), sizeof(n_a)*2, sizeof(n_a)*3) == sizeof(n_a)*3*8,
	"structure assignment");

  check(i.m1 == 77, "i.m1");
  check(i.m2 == 1924, "i.m2");
  check(i.m3 == -1471601920, "i.m3");
  check(vec_all_eq(i.m5, ((vector float){-1.38e+09, 5.96e+08, 6.88e+08, -3.2e+08})),
	"i.m5");

  initn_a(77, 1924, -1471601920, 3.65e+18, ((vector float){-1.38e+09, 5.96e+08, 6.88e+08, -3.2e+08}));
  varargsn_a(77, 1924, -1471601920, 3.65e+18, ((vector float){-1.38e+09, 5.96e+08, 6.88e+08, -3.2e+08}));

  gn_a.m1 = 77;
  gn_a.m2 = 1924;
  gn_a.m3 = -1471601920;
  gn_a.m4 = 3.65e+18;
  gn_a.m5 = ((vector float){-1.38e+09, 5.96e+08, 6.88e+08, -3.2e+08});
  a = valuen_a();

  check(a.m1 == 77, "a.m1");
  check(a.m2 == 1924, "a.m2");
  check(a.m3 == -1471601920, "a.m3");
  check(vec_all_eq(a.m5, ((vector float){-1.38e+09, 5.96e+08, 6.88e+08, -3.2e+08})),
	"a.m5");

  p = addrn_a();

  check(p->m1 == 77, "p->m1");
  check(p->m2 == 1924, "p->m2");
  check(p->m3 == -1471601920, "p->m3");
  check(vec_all_eq(p->m5, ((vector float){-1.38e+09, 5.96e+08, 6.88e+08, -3.2e+08})),
	"p->m5");

  eqn_a(&a);

  check(gn_a.m1 == 77, "gn_a.m1");
  check(gn_a.m2 == 1924, "gn_a.m2");
  check(gn_a.m3 == -1471601920, "gn_a.m3");
  check(vec_all_eq(gn_a.m5, ((vector float){-1.38e+09, 5.96e+08, 6.88e+08, -3.2e+08})),
	"gn_a.m5");

  getsn_a(&v[0]);
  v[2].m1 = v[0].m1;
  v[2].m2 = v[0].m2;
  v[2].m3 = v[0].m3;
  v[2].m4 = v[0].m4;
  v[2].m5 = v[0].m5;

  check(v[2].m1 == 77, "v[2].m1");
  check(v[2].m2 == 1924, "v[2].m2");
  check(v[2].m3 == -1471601920, "v[2].m3");
  check(vec_all_eq(v[2].m5, ((vector float){-1.38e+09, 5.96e+08, 6.88e+08, -3.2e+08})),
	"v[2].m5");
}
