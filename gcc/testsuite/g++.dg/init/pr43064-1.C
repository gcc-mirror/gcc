/* Verify that errors about member initializers appear at the bad value,
   rather than on the last token of the final initializer.  */

// { dg-do compile }
// { dg-options "-fdiagnostics-show-caret" }

class X {
  X() : bad(42), // { dg-error "invalid conversion from 'int' to 'void\\*'" }
	good(42)
  { }
  
  void* bad;
  int good;

  /* { dg-begin-multiline-output "" }
   X() : bad(42),
             ^~
             |
             int
     { dg-end-multiline-output "" } */
};

class Y {
  Y() : bad(-1), // { dg-error "invalid conversion from 'int' to 'void\\*'" }
	good(42)
  { }
  
  void* bad;
  int good;

  /* { dg-begin-multiline-output "" }
   Y() : bad(-1),
             ^~
             |
             int
     { dg-end-multiline-output "" } */
};
