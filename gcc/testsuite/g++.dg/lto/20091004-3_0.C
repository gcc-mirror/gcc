// { dg-lto-do assemble }
// { dg-lto-options {{-O -flto}} }

extern "C" double sqrt (double __x) throw ();
typedef double VECTOR[3];
enum { X = 0,  Y = 1,  Z = 2,  T = 3 };
inline void VLength(double& a, const VECTOR b)
{
  a = sqrt(b[X] * b[X] + b[Y] * b[Y] + b[Z] * b[Z]);
}
void
determine_subpatch_flatness(void)
{
  double temp1;
  VECTOR TempV;
  VLength(temp1, TempV);
  VLength(temp1, TempV);
}
