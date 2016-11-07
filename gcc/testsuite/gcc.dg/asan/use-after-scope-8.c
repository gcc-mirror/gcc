// { dg-do compile }
// { dg-additional-options "-fdump-tree-asan0" }
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

int
fn1 ()
{
  int x = 123;
  register int a asm("rdi") = 123;

  return x * x;
}

/* { dg-final { scan-tree-dump-not "ASAN_CHECK" "asan0" } }  */
