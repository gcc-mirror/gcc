/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized -fno-inline --param ipa-cp-eval-threshold=100" } */
/* { dg-add-options bind_pic_locally } */

int
very_long_function(int a)
{
  if (a > 0)
    return 2 * a + very_long_function (a)/4;
  else
    return 2 * -a + very_long_function (a)/4;
}

int
blah ()
{
  very_long_function (1);
}
/* One appearance for dump, one self recursive call and one call from main.  */
/* { dg-final { scan-tree-dump-times "very_long_function.constprop \\(\\)" 3 "optimized"} } */
