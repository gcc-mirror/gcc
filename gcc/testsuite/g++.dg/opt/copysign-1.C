// { dg-options "-O2" }
// { dg-do compile }
// PR rtl-opt/27883
// MIPS used to ICE because local flow update
// was not removing an invalid REG_DEAD.


double copysign (double x, double y);
double GetDouble();
double a = copysign (1.0, GetDouble());
