/* PR 34808 */
/* { dg-do compile }
/* { dg-options "-fno-tree-dominator-opts" } */

extern int flags;

struct r { int code; int val;};

int
foo (struct r *home)
{
  int n = 0;
  int regno = -1;

  if (home->code == 0)
    regno = home->val;

  if (home->code == 1)
      bar ();
  else if (regno >= 0)
    n = (regno == 16
	 ? 16
	 : (regno - (unsigned long long) (flags != 0 ? 63 : 15)
	    ? regno - 128
	    : -1));

  baz ();
  return n;
}
