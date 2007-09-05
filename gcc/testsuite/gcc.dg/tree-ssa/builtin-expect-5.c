/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop" } */

f (int i, float j) 
{ 
  if (__builtin_expect (i > 0 && __builtin_expect (j != 0, 1), 0))
    a ();
  else
    b ();
} 

/* { dg-final { scan-tree-dump-times { if } 2 "forwprop1"} } */
/* { dg-final { scan-tree-dump {builtin_expect[^\n]*, 0\);\n[^\n]*if} "forwprop1"} } */
/* { dg-final { scan-tree-dump {builtin_expect[^\n]*, 1\);\n[^\n]*if} "forwprop1"} } */
/* { dg-final { cleanup-tree-dump "forwprop?" } } */
