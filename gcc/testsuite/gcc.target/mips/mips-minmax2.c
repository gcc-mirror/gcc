/* { dg-do compile } */
/* { dg-options "-mhard-float -march=mips32r6" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

extern double fmin (double, double);
extern double fmax (double, double);
extern float fminf (float, float);
extern float fmaxf (float, float);

/* Test MIN.D.  */

/* { dg-final { scan-assembler "\tmin\\.d\t" } } */
double test01 (double x, double y) {
    return fmin (x, y);
}

/* Test MIN.S.  */

/* { dg-final { scan-assembler "\tmin\\.s\t" } } */
float test02 (float x, float y) {
    return fminf (x, y);
}

/* Test MAX.D.  */

/* { dg-final { scan-assembler "\tmax\\.d\t" } } */
double test03 (double x, double y) {
    return fmax (x, y);
}

/* Test MAX.S.  */

/* { dg-final { scan-assembler "\tmax\\.s\t" } } */
float test04 (float x, float y) {
    return fmaxf (x, y);
}
