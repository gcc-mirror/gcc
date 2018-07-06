/* { dg-do compile } */
/* { dg-options "-ffile-prefix-map==FILE-PREFIX" } */

#pragma message "FILE starts with " __FILE__       /* { dg-message "FILE starts with FILE-PREFIX" } */
#pragma message "BASE_FILE starts with " __BASE_FILE__  /* { dg-message "BASE_FILE starts with FILE-PREFIX" } */
