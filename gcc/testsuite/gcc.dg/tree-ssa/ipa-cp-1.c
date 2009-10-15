/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized -fno-inline" } */
/* { dg-add-options bind_pic_locally } */

int
very_long_function(int a)
{
  return very_long_function (a)/4;
}
main()
{
  very_long_function (1);
}
/* One appereance for dump, one self recursive call and one call from main.  */
/* { dg-final { scan-tree-dump-times "very_long_function.clone.0 \\(\\)" 3 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
