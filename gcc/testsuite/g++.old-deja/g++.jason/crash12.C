// { dg-do assemble  }
// Bug: g++ dies instead of flagging this invalid.

inline float  max(float  x, float  y) { return (x>y)?x:y; }

float  b(float  x, float  y, float  z)
{
  float f = (y<x)?x:(max<y)?z:y;	// { dg-error "" } 
  return f;
}
