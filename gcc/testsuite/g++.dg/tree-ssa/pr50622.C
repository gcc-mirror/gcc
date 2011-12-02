// { dg-do compile }
// { dg-options "-O2" }

typedef __complex__ double Value;
struct LorentzVector
{
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
};

inline LorentzVector
operator+(LorentzVector a, const LorentzVector & b) {
  return a += b;
}

Value ex, et;
LorentzVector sum() {
  LorentzVector v1; v1.theX =ex; v1.theY =ex+et; v1.theZ =ex-et;   v1.theT =et;
  return v1+v1;
}
