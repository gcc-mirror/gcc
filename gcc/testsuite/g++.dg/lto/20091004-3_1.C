extern "C" double sqrt (double __x) throw ();
typedef double VECTOR[3];
enum { X = 0,  Y = 1,  Z = 2,  T = 3 };
inline void VLength(double& a, const VECTOR b)
{
  a = sqrt(b[X] * b[X] + b[Y] * b[Y] + b[Z] * b[Z]);
}
int
All_Torus_Intersections(void)
{
  double len;
  VECTOR D;
  VLength(len, D);
  VLength(len, D);
}

