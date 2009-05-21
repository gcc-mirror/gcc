/* { dg-do compile */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "fmpy" } } */
/* { dg-final { scan-assembler-not "fadd" } } */
/* { dg-final { scan-assembler-not "fsub" } } */
/* { dg-final { scan-assembler "fma" } } */
/* { dg-final { scan-assembler "fms" } } */
/* { dg-final { scan-assembler "fnma" } } */

float foo01(float a, float b, float c) {return (a + b * c);}
float foo02(float a, float b, float c) {return (a - b * c);}
float foo03(float a, float b, float c) {return (a * b + c);}
float foo04(float a, float b, float c) {return (a * b - c);}

double foo05(double a, double b, double c) {return (a + b * c);}
double foo06(double a, double b, double c) {return (a - b * c);}
double foo07(double a, double b, double c) {return (a * b + c);}
double foo08(double a, double b, double c) {return (a * b - c);}

__float80 foo09(__float80 a, __float80 b, __float80 c) {return (a + b * c);}
__float80 foo10(__float80 a, __float80 b, __float80 c) {return (a - b * c);}
__float80 foo11(__float80 a, __float80 b, __float80 c) {return (a * b + c);}
__float80 foo12(__float80 a, __float80 b, __float80 c) {return (a * b - c);}



float foo20(double a, double b, double c) {return (float) (a + b * c);}
float foo21(double a, double b, double c) {return (float) (a - b * c);}
float foo22(double a, double b, double c) {return (float) (a * b + c);}
float foo23(double a, double b, double c) {return (float) (a * b - c);}

float foo24(__float80 a, __float80 b, __float80 c) {return (float) (a + b * c);}
float foo25(__float80 a, __float80 b, __float80 c) {return (float) (a - b * c);}
float foo26(__float80 a, __float80 b, __float80 c) {return (float) (a * b + c);}
float foo27(__float80 a, __float80 b, __float80 c) {return (float) (a * b - c);}

double foo28(__float80 a, __float80 b, __float80 c) {return (double) (a + b * c);}
double foo29(__float80 a, __float80 b, __float80 c) {return (double) (a - b * c);}
double foo30(__float80 a, __float80 b, __float80 c) {return (double) (a * b + c);}
double foo31(__float80 a, __float80 b, __float80 c) {return (double) (a * b - c);}


float foo001(float a, float b, double c) { return (a + b * c); }
float foo002(float a, float b, double c) { return (a - b * c); }

float foo005(float a, double b, double c) { return (a + b * c); }
float foo006(float a, double b, double c) { return (a - b * c); }
float foo007(float a, double b, double c) { return (a * b + c); }
float foo008(float a, double b, double c) { return (a * b - c); }

double foo009(double a, float b, double c) { return (a + b * c); }
double foo010(double a, float b, double c) { return (a - b * c); }
double foo011(double a, float b, double c) { return (a * b + c); }
double foo012(double a, float b, double c) { return (a * b - c); }

float foo013(float a, double b, __float80 c) { return (a + b * c); }
float foo014(float a, double b, __float80 c) { return (a - b * c); }
float foo017(double a, float b, __float80 c) { return (a + b * c); }
float foo018(double a, float b, __float80 c) { return (a - b * c); }

float foo021(float a, __float80 b, double c) { return (a + b * c); }
float foo022(float a, __float80 b, double c) { return (a - b * c); }
float foo023(float a, __float80 b, double c) { return (a * b + c); }
float foo024(float a, __float80 b, double c) { return (a * b - c); }
