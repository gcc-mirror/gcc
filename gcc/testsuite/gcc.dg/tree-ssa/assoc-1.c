/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized-raw -fno-tree-reassoc" } */

int f0(int a,int b,int c){
  int d = a + b;
  int e = c + b;
  return d - e;
}
int f1(int a,int b,int c){
  int d = a + b;
  int e = b - c;
  return d - e;
}
int f2(int a,int b,int c){
  int d = a + b;
  int e = c - b;
  return e + d;
}
int f3(int a,int b,int c){
  int d = a - b;
  int e = c - b;
  return d - e;
}
int f4(int a,int b,int c){
  int d = b - a;
  int e = c - b;
  return e + d;
}

/* { dg-final { scan-tree-dump-times "plus_expr" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "minus_expr" 3 "optimized" } } */
