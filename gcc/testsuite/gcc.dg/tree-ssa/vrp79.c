/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#ifdef __SIZEOF_INT128__
typedef unsigned __int128 NT;
#else
typedef unsigned long long NT;
#endif

extern void do_not_go_away ();

void f (NT x, NT y)
{
  NT n = 1;
  n <<= (__CHAR_BIT__ * sizeof (NT) - 1);
  if (x > n) return;
  if (y > n) return;
  NT z = x + y;
  if (z == 42) do_not_go_away ();
}

/* { dg-final { scan-tree-dump "do_not_go_away" "optimized" } } */
