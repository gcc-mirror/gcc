/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-dominator-opts -fdump-tree-vrp1" } */

int f(int x, int y)
{ 
  int ret; 
  if (x == y)
    ret = x ^ y;
  else
    ret = 1;

  return ret;
} 

/* We should have computed x ^ y as zero and propagated the result into the
   PHI feeding the result.  */

/* { dg-final { scan-tree-dump "ret_\[0-9\]+ = PHI <\[01\]\\\(\[0-9\]+\\\), \[01\]\\\(\[0-9\]+\\\)>" "vrp1" } } */
