
/* { dg-do compile } */
/* { dg-options "-O3 -Wunused-parameter" } */
static int t(int i) /* { dg-warning "unused parameter" "unused parameter warning" } */
{
  return 0;
}
int tt()
{
  return t(0);
}
