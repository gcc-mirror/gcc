/* { dg-options "--param asan-globals=0 -fdump-tree-asan1 -ffat-lto-objects" } */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */

extern int a;

int foo ()
{
  return a;
}

/* { dg-final { scan-tree-dump-times "ASAN_CHECK" 0 "asan1" } } */
