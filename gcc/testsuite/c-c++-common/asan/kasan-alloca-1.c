/* { dg-do compile } */
/* { dg-options "-fno-sanitize=address -fsanitize=kernel-address -fdump-tree-sanopt" } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

void foo(int index, int len) {
  char str[len];
  str[index] = '1'; // BOOM
}

/* { dg-final { scan-tree-dump-not "__builtin___asan_alloca_poison" "sanopt" } } */
/* { dg-final { scan-tree-dump-not "__builtin___asan_allocas_unpoison" "sanopt" } } */
