/* { dg-do compile } */
/* { dg-options "-fsanitize=address -fsanitize-sections=.xxx,.yyy -fdump-tree-sanopt" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */

int x __attribute__((section(".xxx"))) = 1;
int y __attribute__((section(".yyy"))) = 1;
int z __attribute__((section(".zzz"))) = 1;

/* { dg-final { scan-tree-dump "__builtin___asan_unregister_globals \\(.*, 2\\);" "sanopt" } } */

