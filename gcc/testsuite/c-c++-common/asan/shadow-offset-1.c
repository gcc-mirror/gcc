/* { dg-do compile } */
/* { dg-options "-fno-sanitize=address -fsanitize=kernel-address --param asan-instrumentation-with-call-threshold=100 -fasan-shadow-offset=12345 -fdump-tree-sanopt" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
 
int f (int *p)
{
  return *p;
}

/* { dg-final { scan-tree-dump "12345" "sanopt" }  } */
