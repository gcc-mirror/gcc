/* PR c/9928 */
/* { dg-do compile } */

enum { CODES }; /* { dg-error "previous declaration" } */
enum { CODES }; /* { dg-error "conflicting types" } */
