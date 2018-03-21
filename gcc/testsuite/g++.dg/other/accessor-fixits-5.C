// PR c++/84892
// { dg-options "-fdiagnostics-show-caret" }

class S {
private:
  bool field;

public:
  bool get_field() const {
    return field;
  }
};

bool thingy(const S & s) {
  return s.field; // { dg-error "'bool S::field' is private within this context" }
  /* { dg-begin-multiline-output "" }
   return s.field;
            ^~~~~
     { dg-end-multiline-output "" } */

  // { dg-message "declared private here" "" { target *-*-* } 6 }
  /* { dg-begin-multiline-output "" }
   bool field;
        ^~~~~
     { dg-end-multiline-output "" } */
 
  // { dg-message "field 'bool S::field' can be accessed via 'bool S::get_field\\(\\) const'" "" { target *-*-* } .-12 }
  /* { dg-begin-multiline-output "" }
   return s.field;
            ^~~~~
            get_field()
     { dg-end-multiline-output "" } */
}
