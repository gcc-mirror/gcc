/* { dg-do run } */

/* Testcase distilled from glibc's nss_parse_service_list in nss/nsswitch.c
   It can't be distilled further.  Fails with `-O2' for i[3456]86.  */

/* this simulates a bounded-pointer type.  */
struct ucharp { unsigned char *v, *l, *h; };

/* this simulates bounded-pointer check prior to pointer dereference.  */
#define AREF(var, idx) ((((((((var).v+(idx)) < (var).l) \
			   || (((var).v+(idx)+1) > (var).h))) \
			  && (__builtin_trap (), 0)), \
			 (var).v)[(idx)])

struct list
{
  struct list *next;
};

struct list *
alloc_list (void)
{
  static struct list l;
  return &l;
}

int one = 1;

void
foo (struct ucharp cp, struct ucharp lp, struct list **nextp)
{
  while (1)
    {
      struct list *list;
      while (AREF (lp, 0) && AREF (cp, AREF (lp, 0)))
        ++lp.v;
      list = alloc_list ();
      while (AREF (cp, AREF (lp, 0)))
        ++lp.v;
      if (AREF (lp, 0) == one)
	do
	  ++lp.v;
	while (AREF (lp, 0) && AREF (cp, AREF (lp, 0)));
      /* The above AREF (cp, ...) fails because the pseudo created to
	 hold cp.v holds garbage, having never been set.
	 The easiest way to see the problem is to compile wiht `-O2 -da'
	 then look at *.09.loop.  Search for something like this:

	 Hoisted regno 183 r/o from (mem/s:SI (reg:SI 16 argp) 10)
	   Replaced reg 91, deleting init_insn (213).

	 Now, look for the use of reg 91, which has no set.  */

      *nextp = list;
      nextp = &list->next;
      if (!*lp.v)
	break;
    }
}

extern void exit (int);

int
main (void)
{
  static unsigned char cp0[] = "\0\0\0\0";
  struct ucharp cp = { cp0, cp0, cp0 + sizeof (cp0) };

  static unsigned char lp0[] = "\1\1\0\0";
  struct ucharp lp = { lp0, lp0, lp0 + sizeof (lp0) };

  struct list list;
  struct list *nextp = &list;

  foo (cp, lp, &nextp);

  exit (0);
}
