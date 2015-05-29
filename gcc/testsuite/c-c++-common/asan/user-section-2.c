/* { dg-do compile } */
/* { dg-options "-fsanitize=address -fsanitize-sections=.x* -fdump-tree-sanopt" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */

int x __attribute__((section(".x1"))) = 1;
int y __attribute__((section(".x2"))) = 1;
int z __attribute__((section(".x3"))) = 1;

/* { dg-final { scan-tree-dump "__builtin___asan_unregister_globals \\(.*, 3\\);" "sanopt" } } */

