/* { dg-do run } */
/* { dg-options "-O2 -fPIC -m32" } */

void (*pfn)(void);

struct s
{
  void** q;
  int h;
  int t;
  int s;
};


void* f (struct s *, struct s *) __attribute__ ((noinline, regparm(1)));

void*
__attribute__ ((regparm(1)))
f (struct s *p, struct s *p2)
{
  void *gp, *gp1;
  int t, h, s, t2, h2, c, i;

  if (p2->h == p2->t)
    return 0;

  (*pfn) ();

  h = p->h;
  t = p->t;
  s = p->s;

  h2 = p2->h;
  t2 = p2->t;

  gp = p2->q[h2++];

  c = (t2 - h2) / 2;
  for (i = 0; i != c; i++)
    {
      if (t == h || (h == 0 && t == s - 1))
	break;
      gp1 = p2->q[h2++];
      p->q[t++] = gp1;
      if (t == s)
	t = 0;
    }

  p2->h = h2;
  return gp;
}

static void gn () { }

int
main()
{
  struct s s1, s2;
  void *q[10];

  pfn = gn;

  s1.q = q;
  s1.h = 0;
  s1.t = 2;
  s1.s = 4;

  s2.q = q;
  s2.h = 0;
  s2.t = 4;
  s2.s = 2;

  f (&s1, &s2);

  return 0;
}
