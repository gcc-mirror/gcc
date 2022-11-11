void test_direct (void)
{
  test_direct (); /* { dg-warning "infinite recursion" } */
}

void test_guarded (int flag)
{
  if (flag)
    test_guarded (flag); /* { dg-warning "infinite recursion" } */
}

void test_flipped_guard (int flag)
{
  if (flag)
    test_guarded (!flag); /* { dg-bogus "infinite recursion" } */
}

void test_param_variant (int depth)
{
  if (depth > 0)
    test_param_variant (depth - 1); /* { dg-bogus "infinite recursion" } */
}

void test_unguarded_param_variant (int depth)
{
  /* We fail to report this: we see that depth is being decremented,
     but don't notice that every path through the function is
     recursing.  */
  test_unguarded_param_variant (depth - 1); /* { dg-warning "infinite recursion" "TODO" { xfail *-*-* } } */
}

int g;

void test_global_variant ()
{
  if (g-- > 0)
    test_global_variant (); /* { dg-bogus "infinite recursion" } */
}

/* This is a bounded recursion, as "n" is decremented before recursing... */

int test_while_do_predecrement_param (int n)
{
  int x = 0;
  while (n)
    x += test_while_do_predecrement_param (--n); /* { dg-bogus "infinite recursion" } */
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
    x += test_do_while_predecrement_param (--n); /* { dg-bogus "infinite recursion" } */
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
  /* We catch this; the "n--" means we recurse with the
     same value for the 2nd param.  */
  if (flag) /* { dg-message "when 'flag != 0'" } */
    test_partially_guarded_postdecrement (flag, n--); /* { dg-warning "infinite recursion" } */
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
