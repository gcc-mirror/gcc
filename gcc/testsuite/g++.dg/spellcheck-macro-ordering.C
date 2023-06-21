// PR c++/72786

/* Example of a macro-ordering issue, where the use is before the defn.  */

class DocTargetDriver {
  virtual void clone() const OVERRIDE { }
  /* Offering "OVERRIDE" as a spelling suggestion for "OVERRIDE" would be
     nonsensical.  */
  // { dg-bogus "did you mean" "" { target *-*-* } .-3 }
  // { dg-error "expected .;. at end of member declaration" "" { target *-*-* } .-4 }
  // { dg-error ".OVERRIDE. does not name a type" "" { target *-*-* } .-5 }
  // { dg-message "the macro 'OVERRIDE' had not yet been defined" "" { target *-*-* } .-6 }
};

#define OVERRIDE override
// { dg-message "9:it was later defined here" "" { target *-*-* } .-1 }
