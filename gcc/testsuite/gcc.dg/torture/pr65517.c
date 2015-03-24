/* { dg-do compile } */

typedef void (*argmatch_exit_fn)();
int a;
void __argmatch_die () { __builtin_exit (0); }

int
main ()
{
  while (1)
    {
      argmatch_exit_fn b = __argmatch_die;
      if (a)
	b ();
    }
  return 0;
}
