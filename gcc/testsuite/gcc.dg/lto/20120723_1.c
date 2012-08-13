/* Make sure that by reference and by value aggregate jump functions do not get
   mixed up.  */

extern void abort (void);

struct S
{
  int i;
  void (*f)(struct S *);
  int j;
};

struct E
{
  struct S *p;
};

extern struct S *gs;
extern int gr;
extern char gc[1024];

static __attribute__ ((noinline, noclone)) struct S *
get_s (void)
{
  return (struct S *) &gc;
}

static void good_target (struct S *s)
{
  gr = 0;
}

extern void bar (struct E e);

void foo (struct E e)
{
  gs->f = good_target;
  bar (e);
}
