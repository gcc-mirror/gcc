/* PR c/9928 */
/* { dg-do compile } */

enum { CODES }; /* { dg-message "note: previous definition" } */
enum { CODES }; /* { dg-error "conflicting types|redeclaration of enumerator" } */
