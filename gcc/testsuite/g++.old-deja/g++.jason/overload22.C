// { dg-do assemble  }
// PRMS Id: 4574
// Bug: g++ prefers int to double for float& argument

inline double abs (double x) { return x;}
inline int    abs (int i)    { return i; }

float& fn(float& f)
{
  return f;
}

void foo()
{
  float f = 23.45;
  abs(fn(f));			// gets bogus warning
}
