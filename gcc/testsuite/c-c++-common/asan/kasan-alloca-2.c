/* { dg-do compile } */
/* { dg-options "-fno-sanitize=address -fsanitize=kernel-address --param asan-instrument-allocas=1 --param asan-stack=1 -fdump-tree-sanopt" } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

void foo(int index, int len) {
  char str[len];
  str[index] = '1'; // BOOM
}

/* { dg-final { scan-tree-dump-times "__builtin___asan_alloca_poison" 1 "sanopt" } } */
/* { dg-final { scan-tree-dump-times "__builtin___asan_allocas_unpoison" 1 "sanopt" } } */
