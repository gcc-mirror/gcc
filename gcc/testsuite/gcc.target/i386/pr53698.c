/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O -mx32 -maddress-mode=long -fno-tree-dominator-opts" } */

extern char foo[];

void
test2 (void)
{
  int s;
  for (s = 0;; ++s)
    {
      if (foo[s] != s)
	__builtin_abort ();
      foo[s] = s;
    }
}
