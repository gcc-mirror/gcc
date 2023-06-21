/* { dg-do preprocess } */
/* { dg-additional-options "-fdiagnostics-show-caret -Wunused-macros -Wbuiltin-macro-redefined" } */

/* Verify that we output range information for diagnostics involving
   macro definitions.  */

#undef __TIME__ /* { dg-warning {undefining "__TIME__"} } */
/* { dg-begin-multiline-output "" }
 #undef __TIME__
        ^~~~~~~~
/* { dg-end-multiline-output "" } */

#define XYZ 123 /* { dg-warning {macro "XYZ" is not used} } */
/* { dg-begin-multiline-output "" }
 #define XYZ 123
         ^~~
/* { dg-end-multiline-output "" } */

#define MACRO initial_definition /* { dg-line def_line } */

/* This locus is output first for the unused warning... */
/* { dg-warning {macro "MACRO" is not used} "" { target *-*-* } def_line } */
/* { dg-begin-multiline-output "" }
 #define MACRO initial_definition
         ^~~~~
/* { dg-end-multiline-output "" } */

/* ...then a second time for the redefinition warning.  */
/* { dg-note {this is the location of the previous definition} "" { target *-*-* } def_line } */
/* { dg-begin-multiline-output "" }
 #define MACRO initial_definition
         ^~~~~
/* { dg-end-multiline-output "" } */

#define MACRO /* { dg-warning {"MACRO" redefined} } */
/* { dg-begin-multiline-output "" }
 #define MACRO
         ^~~~~
{ dg-end-multiline-output "" } */

#define MACRO2(x,y) x /* { dg-note {macro "MACRO2" defined here} } */
/* { dg-begin-multiline-output "" }
 #define MACRO2(x,y)
         ^~~~~~
{ dg-end-multiline-output "" } */

MACRO2(MACRO, MACRO)
MACRO2(MACRO) /* { dg-error {macro "MACRO2" requires 2 arguments, but only 1 given} } */
/* { dg-begin-multiline-output "" }
 MACRO2(MACRO)
             ^
{ dg-end-multiline-output "" } */
