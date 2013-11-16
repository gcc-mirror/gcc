/* { dg-do run } */
/* { dg-options "-g" } */
/* { dg-skip-if "" { *-*-* }  { "*" } { "-O0" } } */

void
foo (int i)
{
  int j = 0;
  i = 1;
  j = j + 1;  /* BREAK */
}

int
main (void)
{
  foo (0);
  return 0;
}

/* { dg-final { gdb-test 10 "i" "1" } } */