// PR c++/72786

/* Example of undeffed macro.  */

#define OVERRIDE override

#undef OVERRIDE

class DocTargetDriver {
  virtual void clone() const OVERRIDE { } // { dg-line usage }
  /* Offering "OVERRIDE" as a spelling suggestion for "OVERRIDE" would be
     nonsensical.  */
  // { dg-bogus "did you mean" "" { target *-*-* } usage }
  // { dg-error "expected .;. at end of member declaration" "" { target *-*-* } usage }
  // { dg-error ".OVERRIDE. does not name a type" "" { target *-*-* } usage }
  // { dg-bogus "macro" "" { target *-*-* } usage }
};
