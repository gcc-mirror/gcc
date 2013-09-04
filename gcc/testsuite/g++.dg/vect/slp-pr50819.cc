/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */

typedef float Value;

struct LorentzVector
{

  LorentzVector(Value x=0, Value  y=0, Value  z=0, Value  t=0) :
theX(x),theY(y),theZ(z),theT(t){}
  LorentzVector & operator+=(const LorentzVector & a) {
    theX += a.theX;
    theY += a.theY;
    theZ += a.theZ;
    theT += a.theT;
    return *this;
  }

  Value theX;
  Value theY;
  Value theZ;
  Value theT;
}  __attribute__ ((aligned(16)));

inline LorentzVector
operator+(LorentzVector const & a, LorentzVector const & b) {
  return
LorentzVector(a.theX+b.theX,a.theY+b.theY,a.theZ+b.theZ,a.theT+b.theT);
}

inline LorentzVector
operator*(LorentzVector const & a, Value s) {
    return LorentzVector(a.theX*s,a.theY*s,a.theZ*s,a.theT*s);
}

inline LorentzVector
operator*(Value s, LorentzVector const & a) {
  return a*s;
}


void sum1(LorentzVector & res, Value s, LorentzVector const & v1, LorentzVector
const & v2) {
  res += s*(v1+v2);
}

void sum2(LorentzVector & res, Value s, LorentzVector const & v1, LorentzVector
const & v2) {
  res = res + s*(v1+v2);
}

/* { dg-final { scan-tree-dump-times "basic block vectorized" 2 "slp" } } */
/* { dg-final { cleanup-tree-dump "slp" } } */
