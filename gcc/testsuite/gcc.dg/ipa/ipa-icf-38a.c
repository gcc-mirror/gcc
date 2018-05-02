/* { dg-skip-if "" { *-*-* } } */

typedef int v4si __attribute__ ((vector_size (16)));

__attribute__ ((noinline))
int bar(void)
{
  v4si a = {1,2,3,4};
  v4si b = {3,2,1,4};
  v4si c;

  return 54;
}

