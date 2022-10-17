/* Verify that we use precise locations when emitting diagnostics
   about pragmas.  */

/* { dg-do assemble } */
/* { dg-options "-fdiagnostics-show-caret" } */

/* pack ****************************************************************************/

#pragma pack
/* { dg-warning "missing '\\(' after '#pragma pack' - ignored" "" { target *-*-* } .-1 }
   { dg-begin-multiline-output "" }
 #pragma pack
         ^~~~
   { dg-end-multiline-output "" }  */

#pragma pack (
/* { dg-warning "malformed '#pragma pack' - ignored" "" { target *-*-* } .-1 }
   { dg-begin-multiline-output "" }
 #pragma pack (
         ^~~~
   { dg-end-multiline-output "" }  */

#pragma pack (32
/* { dg-warning "malformed '#pragma pack' - ignored" "" { target *-*-* } .-1 }
   { dg-begin-multiline-output "" }
 #pragma pack (32
         ^~~~
   { dg-end-multiline-output "" }  */

#pragma pack (3.14159
/* { dg-warning "invalid constant in '#pragma pack' - ignored" "" { target *-*-* } .-1 }
   { dg-begin-multiline-output "" }
 #pragma pack (3.14159
               ^~~~~~~
   { dg-end-multiline-output "" }  */

#pragma pack (push, 3.14159
/* { dg-warning "invalid constant in '#pragma pack' - ignored" "" { target *-*-* } .-1 }
   { dg-begin-multiline-output "" }
 #pragma pack (push, 3.14159
                     ^~~~~~~
   { dg-end-multiline-output "" }  */

#pragma pack (toothbrush
/* { dg-warning "unknown action 'toothbrush' for '#pragma pack' - ignored" "" { target *-*-* } .-1 }
   { dg-begin-multiline-output "" }
 #pragma pack (toothbrush
               ^~~~~~~~~~
   { dg-end-multiline-output "" }  */

#pragma pack() pyjamas
/* { dg-warning "junk at end of '#pragma pack'" "" { target *-*-* } .-1 }
   { dg-begin-multiline-output "" }
 #pragma pack() pyjamas
                ^~~~~~~
   { dg-end-multiline-output "" }  */

/* target ****************************************************************************/

#pragma GCC target 42
/* { dg-warning "#pragma GCC option' is not a string" "" { target *-*-* } .-1 }
   { dg-begin-multiline-output "" }
 #pragma GCC target 42
                    ^~
   { dg-end-multiline-output "" }  */

#pragma GCC target ( 1776
/* { dg-warning "#pragma GCC option' is not a string" "" { target *-*-* } .-1 }
   { dg-begin-multiline-output "" }
 #pragma GCC target ( 1776
                      ^~~~
   { dg-end-multiline-output "" }  */

/* message ****************************************************************************/

#pragma message "foo" int
/* { dg-warning "junk at end of '#pragma message'" "" { target *-*-* } .-1 }
   { dg-message "'#pragma message: foo'" "" { target *-*-* } .-2 }
   { dg-begin-multiline-output "" }
 #pragma message "foo" int
                       ^~~
   { dg-end-multiline-output "" }
   { dg-begin-multiline-output "" }
 #pragma message "foo" int
         ^~~~~~~
   { dg-end-multiline-output "" }  */
