/* { dg-do compile } */
/* { dg-options "-O -fno-tree-forwprop -fno-tree-ccp -fdump-tree-fre1-details" } */

_Complex float m;

void
foo (_Complex float x)
{    
  float r = __real x;
  float i = __imag x;
  _Complex float z;
  __real z = r;
  __imag z = i;
  m = z;
} 

void
bar (_Complex float x)
{    
  float r = __real x;
  float i = __imag x;
  _Complex float z = x;
  __real z = r;
  __imag z = i;
  m = z;
} 

/* We should CSE all the way to replace the stored value with x.  */
/* { dg-final { scan-tree-dump-times "m = x_\\d\+\\(D\\);" 2 "fre1" } } */
