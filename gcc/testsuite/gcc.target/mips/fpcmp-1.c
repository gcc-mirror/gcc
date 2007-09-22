/* We used to use c.lt.fmt instead of c.ule.fmt here.  */
/* { dg-mips-options "-mhard-float -O2" } */
NOMIPS16 int f1 (float x, float y) { return __builtin_isless (x, y); }
NOMIPS16 int f2 (double x, double y) { return __builtin_isless (x, y); }
/* { dg-final { scan-assembler "c\\.ule\\.s" } } */
/* { dg-final { scan-assembler "c\\.ule\\.d" } } */
