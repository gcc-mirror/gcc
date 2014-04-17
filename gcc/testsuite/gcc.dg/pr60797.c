/* { dg-do compile } */
/* { dg-skip-if "" { alias } } */

extern int foo __attribute__((alias("bar"))); /* { dg-error "supported" } */
int main()
{
  return 0;
}
