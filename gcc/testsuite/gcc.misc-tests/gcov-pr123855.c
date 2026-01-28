/* { dg-options "--coverage -fpath-coverage" } */

__attribute__((__returns_twice__)) void
foo ()
{
  foo ();
  for (;;)
    ;
}

int main () {}

/* { dg-final { run-gcov prime-paths gcov-pr123855.c } } */
