/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1-details" } */

_Complex float 
foo (_Complex float x)
{    
  float r = __real x;
  float i = __imag x;
  _Complex float z;
  __real z = r;
  __imag z = i;
  return z;
} 

_Complex float 
bar (_Complex float x)
{    
  float r = __real x;
  float i = __imag x;
  _Complex float z = x;
  __real z = r;
  __imag z = i;
  return z;
} 

/* We should CSE all the way to replace the final assignment to z with x.  */
/* { dg-final { scan-tree-dump-times "with x_1\\\(D\\\) in z" 3 "fre1" } } */
/* { dg-final { cleanup-tree-dump "fre1" } } */
