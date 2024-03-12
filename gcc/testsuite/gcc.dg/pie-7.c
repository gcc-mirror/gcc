/* { dg-do run { target pie } } */
/* { dg-options "-fno-pie -no-pie" } */
/* { dg-prune-output "-no_pie is deprecated when targeting new OS versions" } */

int main(void)
{
  return 0;
}
