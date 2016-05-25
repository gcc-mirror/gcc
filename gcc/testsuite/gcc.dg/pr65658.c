/* { dg-do compile } */
/* { dg-options "-Wuninitialized -O2 -Wno-implicit" } */
/* { dg-require-effective-target ptr32plus } */

extern int optind;
struct undefinfo
{
  unsigned long l1;
  unsigned long l2;
};
struct undeffoo
{
  char a[64];
  long b[4];
  int c[33];
};
struct problem
{
  unsigned long l1;
  unsigned long l2;
  unsigned long l3;
  unsigned long l4;
};
static unsigned int undef1, undef2, undef3, undef4, undef5, undef6;
static void *undefvp1;
extern struct undefinfo undefinfo;
static int
undefinit1 (void)
{
  struct undeffoo foo;
  int i;
  for (i = 0; i < 2000; i++)
    {
      undef6++;
      external_function5 (((void *) 0), 0, (void *) &foo);
    }
}

static int
undefinit2 (void *problemp, unsigned long problem)
{
  int ret, u;
  if (undefinit1 ())
    return 1;
  if (fn10 ())
    return 1;
  for (u = 0; u < undef6; u++)
    {
      ret = external_function1 (3 + u * 10, 10);
      if (ret)
	return ret;
      external_function6 (0, 0, 0, problemp + problem);
      return 1;
    }
}

static int
fn6 (struct undefinfo *uip, struct problem *problem)
{
  unsigned long amt;
  if (external_function3 (((void *) 0), ((void *) 0), &amt, 0, 0))
    return 1;
  problem->l1 = (unsigned long) undefvp1;
  problem->l4 = uip->l1;
  problem->l3 = uip->l2;
  return 0;
}

static int
setup (void)
{
  struct problem problem;
  if (fn6 (&undefinfo, &problem))
    return 1;
  if (fn2 ())
    return 1;
  if (fn4 (101))
    return 1;
  if (undefinit2 ((void *) problem.l1, problem.l3 * 4))  /* { dg-bogus "problem.l3" "uninitialized variable warning" } */ 
    return 1;
}

int
main (int argc, char **argv)
{
  int optc;
  if (external_function (1))
    return 1;
  if (external_function (1))
    return 1;
  if (external_function (1))
    return 1;
  while ((optc =
	  getopt_long (argc, argv, ((void *) 0), ((void *) 0),
		       ((void *) 0))) != -1)
    {
      switch (optc)
	{
	case 0:
	  break;
	case 'F':
	  external_function (1);
	default:
	  return 1;
	}
    }
  if ((optind != 99))
    {
      return 1;
    }
  setup ();
}
