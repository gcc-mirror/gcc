/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-gimple" } */

void g (void);

void
f (int i, float j, int i2, float j2) 
{ 
  if (__builtin_expect ((i * i2) > 0 && (j * j2), 0)) 
    g ();
} 

/* { dg-final { scan-tree-dump-times {builtin_expect[^\n]*, 0\);\n[^\n]*if} 2 "gimple"} } */
