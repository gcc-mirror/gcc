/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-gimple" } */

void a (void);
void b (void);

void
f (int i, float j, int i2, float j2) 
{ 
  if (__builtin_expect ((i * i2) > 0 && (j * j2), 0))
    a ();
  else
    b ();
} 

/* { dg-final { scan-tree-dump-times {builtin_expect[^\n]*, 0\);\n[^\n]*if} 2 "gimple"} } */
