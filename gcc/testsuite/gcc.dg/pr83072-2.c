/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp-details -fno-tree-forwprop" } */

int f1(int a, int b, int c){
  if(c==0)__builtin_unreachable();
  a *= c;
  b *= c;
  return a == b;
}

int f2(int a, int b, int c){
  c |= 1;
  a *= c;
  b *= c;
  return a == b;
}

/* { dg-final { scan-tree-dump-times "gimple_simplified to" 2 "evrp" } }  */
