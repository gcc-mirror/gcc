// { dg-do assemble  }
// GROUPS passed overloading
typedef int rutBoolean;

class rutBigIntRep
{
public:
  friend rutBoolean operator>(const rutBigIntRep& a, const rutBigIntRep& b);
  operator rutBoolean() const;  
protected:
  enum Kluge {kluge};
  rutBigIntRep(Kluge) {}
  rutBigIntRep();
  rutBigIntRep(const rutBigIntRep& value);
  rutBigIntRep& operator=(const rutBigIntRep& value);
};

rutBoolean operator>(const rutBigIntRep& a, const rutBigIntRep& b) {
  // This should not result in a warning.  It used to warn about the
  // conversion from int to enum while exploring the possibility of
  // converting `a' via `operator rutBoolean', then using the
  // rutBigIntRep(Kluge) constructor.  It later realizes it shouldn't
  // do this, but the call to build_type_conversion was ending up with
  // a warning in convert.
  rutBigIntRep diff(a);
  return 0;
}
