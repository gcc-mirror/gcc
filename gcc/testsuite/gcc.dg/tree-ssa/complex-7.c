/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-forwprop1" } */

float f(_Complex float x, _Complex float y){
  x += y;
  return __builtin_cimagf (x);
}

double g(double x){
  _Complex double c = __builtin_cexpi (x);
  return __builtin_creal (c);
}

/* { dg-final { scan-tree-dump "__builtin_cos" "forwprop1"} } */
/* { dg-final { scan-tree-dump-times "IMAGPART_EXPR" 2 "forwprop1"} } */
