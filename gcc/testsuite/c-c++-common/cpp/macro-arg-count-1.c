/* { dg-options "-fdiagnostics-show-caret" } */

#define MACRO_1(X,Y) /* { dg-line "def_of_MACRO_1" } */
void test_1 ()
{
  MACRO_1(42); /* { dg-line "use_of_MACRO_1" } */
  /* { dg-error "macro 'MACRO_1' requires 2 arguments, but only 1 given" "" { target *-*-* } use_of_MACRO_1 } */
  /* { dg-begin-multiline-output "" }
   MACRO_1(42);
             ^
     { dg-end-multiline-output "" } */
  /* { dg-message "macro .MACRO_1. defined here" "" { target *-*-* } def_of_MACRO_1 }
  /* { dg-begin-multiline-output "" }
 #define MACRO_1(X,Y)
 
     { dg-end-multiline-output "" } */
  /* { dg-error "'MACRO_1' undeclared" "" { target c } use_of_MACRO_1 }
  /* { dg-error "'MACRO_1' was not declared in this scope" "" { target c++ } use_of_MACRO_1 }

  /* { dg-begin-multiline-output "" }
   MACRO_1(42);
   ^~~~~~~
     { dg-end-multiline-output "" } */
  /* { dg-bogus "had not yet been defined" "" { target *-*-* } use_of_MACRO_1 } */
}

#define MACRO_2(X,Y) /* { dg-line "def_of_MACRO_2" } */
void test_2 ()
{
  MACRO_2(1, 2, 3); /* { dg-line "use_of_MACRO_2" } */
  /* { dg-error "macro 'MACRO_2' passed 3 arguments, but takes just 2" "" { target *-*-* } use_of_MACRO_2 } */
  /* { dg-begin-multiline-output "" }
   MACRO_2(1, 2, 3);
                  ^
     { dg-end-multiline-output "" } */
  /* { dg-message "macro .MACRO_2. defined here" "" { target *-*-* } def_of_MACRO_2 }
  /* { dg-begin-multiline-output "" }
 #define MACRO_2(X,Y)
 
     { dg-end-multiline-output "" } */
  /* { dg-error "'MACRO_2' undeclared" "" { target c } use_of_MACRO_2 } */
  /* { dg-error "'MACRO_2' was not declared in this scope" "" { target c++ } use_of_MACRO_2 } */
  /* { dg-begin-multiline-output "" }
   MACRO_2(1, 2, 3);
   ^~~~~~~
     { dg-end-multiline-output "" } */
  /* { dg-bogus "had not yet been defined" "" { target *-*-* } use_of_MACRO_2 } */
}

#define MACRO_3
void test_3 ()
{
  MACRO_3 (42);
}

#define MACRO_4(X,Y)
void test_4 ()
{
  MACRO_4; /* { dg-line "use_of_MACRO_4" } */
  /* { dg-error "'MACRO_4' undeclared" "" { target c } use_of_MACRO_4 } */
  /* { dg-error "'MACRO_4' was not declared in this scope" "" { target c++ } use_of_MACRO_4 } */
  /* { dg-begin-multiline-output "" }
   MACRO_4;
   ^~~~~~~
     { dg-end-multiline-output "" } */
}
