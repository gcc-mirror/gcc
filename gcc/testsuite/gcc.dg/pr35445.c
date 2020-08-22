/* PR c/35445 */
/* { dg-do compile } */

extern int i;
extern int i; /* { dg-message "was here" } */
int i[] = { 0 }; /* { dg-error "conflicting types" } */
