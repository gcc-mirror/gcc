/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef struct { _Bool b; } A;
_Bool f(double u){
  A a;
  if(u==0)
    a.b=1;
  else
    a.b=0;
  return a.b;
}

/* { dg-final { scan-tree-dump-not "VIEW_CONVERT_EXPR" "optimized" } } */
