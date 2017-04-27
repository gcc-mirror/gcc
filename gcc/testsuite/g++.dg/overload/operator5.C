// PR c++/50858

class String {
   typedef struct ImplicitConversionFromWTFStringToBoolDisallowedA* 
   (String::*UnspecifiedBoolTypeA);
   typedef struct ImplicitConversionFromWTFStringToBoolDisallowedB* 
   (String::*UnspecifiedBoolTypeB);
   operator UnspecifiedBoolTypeA() const;
   operator UnspecifiedBoolTypeB() const;
};
inline bool equalIgnoringCase(const String& a, const String& b) { }
inline bool equalPossiblyIgnoringCase(const String& a, 
                                      const String& b,
                                      bool ignoreCase) {
  return ignoreCase ? equalIgnoringCase(a, b) : (a == b); } // { dg-error "ambiguous" }
// { dg-message "note" "note" { target *-*-* } .-1 }
