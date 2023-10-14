/* { dg-do compile } */
/* { dg-options "-std=c90 "} */

void fruit(); /* { dg-message "previous declaration" } */
void fruit( /* { dg-error "conflicting types for" } */
    int b[x], /* { dg-error "undeclared " } */
    short c)
{} /* { dg-message "an argument type that has a" } */
