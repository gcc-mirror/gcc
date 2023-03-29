/* A copy of infinite-recursion-2.c, to see what inlining does to the IR
   when we see it.

   Many cases get converted by the optimizer into iteration, and
   into infinite loops, sometimes trivial ones.

   Right now this is a documented limitation of the warning, but perhaps
   could be readdressed by moving the analyzer earlier.   */

/* { dg-additional-options "-O3" } */

void test_direct (void)
{
  test_direct (); /* Ideally would warn here, but it becomes an infinite loop.  */
}

void test_guarded (int flag)
{
  if (flag)
    test_guarded (flag);  /* Ideally would warn here, but it becomes an infinite loop.  */
}

void test_flipped_guard (int flag)
{
  if (flag)
    test_guarded (!flag);
}

void test_param_variant (int depth)
{
  if (depth > 0)
    test_param_variant (depth - 1);
}

void test_unguarded_param_variant (int depth)
{
  test_unguarded_param_variant (depth - 1); /* Ideally would warn here, but it becomes an infinite loop.  */
}

int g;

void test_global_variant ()
{
  if (g-- > 0)
    test_global_variant ();
}

/* This is a bounded recursion, as "n" is decremented before recursing... */

int test_while_do_predecrement_param (int n)
{
  int x = 0;
  while (n)
    x += test_while_do_predecrement_param (--n);
  return x;
}

/* ...whereas this one is unbounded, as "n" is decremented *after* the
   recursive call, and so is repeatedly called with the same value.  */

int test_while_do_postdecrement_param (int n)
{
  int x = 0;
  while (n)
    x += test_while_do_postdecrement_param (n--); /* { dg-warning "infinite recursion" } */
  return x;
}
/* This is a bounded recursion, as "n" is decremented before recursing... */

int test_do_while_predecrement_param (int n)
{
  int x = 0;
  do
    x += test_do_while_predecrement_param (--n);
  while (--n);
  return x;
}

/* ...whereas this one is unbounded, as "n" is decremented *after* the
   recursive call, and so is repeatedly called with the same value.  */

int test_do_while_postdecrement_param (int n)
{
  int x = 0;
  do
    x += test_do_while_postdecrement_param (n--); /* { dg-warning "infinite recursion" } */
  while (--n);
  return x;
}

/* Various cases of decrementing "n" as the recursion proceeds where
   not every path recurses, but we're not actually checking "n", so
   if "flag" is true it's an infinite recursion.  */

void test_partially_guarded_postdecrement (int flag, int n)
{
  /* Ideally we'd catch this, but it becomes an infinite loop.  */
  if (flag)
    test_partially_guarded_postdecrement (flag, n--);
}

void test_partially_guarded_predecrement (int flag, int n)
{
  /* We fail to report this; we see that "n" is changing,
     though it isn't relevant to whether we recurse.  */
  if (flag)
    test_partially_guarded_predecrement (flag, --n); /* { dg-warning "infinite recursion" "TODO" { xfail *-*-* } } */
}

void test_partially_guarded_subtract (int flag, int n)
{
  /* We fail to report this; we see that "n" is changing,
     though it isn't relevant to whether we recurse.  */
  if (flag)
    test_partially_guarded_subtract (flag, n - 1); /* { dg-warning "infinite recursion" "TODO" { xfail *-*-* } } */
}
