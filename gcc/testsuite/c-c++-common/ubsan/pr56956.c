/* { dg-do run } */
/* { dg-options "-fsanitize=undefined -fsanitize-trap=undefined" } */

unsigned int __attribute__((noinline,noclone))
foo (unsigned int x)
{
  return x <= __INT_MAX__ ? x : -x;
}

int
main ()
{
  volatile unsigned int tem = foo (-__INT_MAX__ - 1);
  return 0;
}
