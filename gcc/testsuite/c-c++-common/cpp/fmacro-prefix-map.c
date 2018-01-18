/* { dg-do compile } */
/* { dg-options "-fmacro-prefix-map==MACRO-PREFIX" } */

#pragma message "FILE starts with " __FILE__       /* { dg-message "FILE starts with MACRO-PREFIX" } */
#pragma message "BASE_FILE starts with " __BASE_FILE__  /* { dg-message "BASE_FILE starts with MACRO-PREFIX" } */
