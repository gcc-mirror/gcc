/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

int f(unsigned x){
  x += 123;
  int y = x;
  y -= 99;
  return y;
}
unsigned g(int x){
  x += 123;
  unsigned y = x;
  y -= 99;
  return y;
}
int h(int x){
  x += __INT_MAX__;
  x += 1;
  return x;
}
int i(int x){
  x += __INT_MAX__;
  x += __INT_MAX__;
  return x;
}

/* { dg-final { scan-tree-dump-times " \\+ 24;" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\(unsigned int\\)" 2 "optimized" { target { ! int16 } } } } */
/* { dg-final { scan-tree-dump-times "\\(unsigned int\\)" 1 "optimized" { target int16 } } } */
/* { dg-final { scan-tree-dump-times "\\(unsigned short\\)" 1 "optimized" { target int16 } } } */
/* { dg-final { scan-tree-dump-not "2147483647" "optimized" } } */
