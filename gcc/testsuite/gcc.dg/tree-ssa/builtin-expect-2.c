/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-gimple" } */

f (int i, float j) 
{ 
  if (__builtin_expect (i > 0 || j, 0))
    ;
  else
    g ();
} 

/* { dg-final { scan-tree-dump-times {builtin_expect[^\n]*, 0\);\n[^\n]*if} 2 "gimple"} } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
