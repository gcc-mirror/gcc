/* We used to use c.lt.fmt instead of c.ule.fmt here.  */
/* { dg-options "isa_rev<=5 -mhard-float" } */
NOMIPS16 int f1 (float x, float y) { return __builtin_isless (x, y); }
NOMIPS16 int f2 (double x, double y) { return __builtin_isless (x, y); }
/* { dg-final { scan-assembler "\tc\\.ule\\.s\t" } } */
/* { dg-final { scan-assembler "\tc\\.ule\\.d\t" } } */
