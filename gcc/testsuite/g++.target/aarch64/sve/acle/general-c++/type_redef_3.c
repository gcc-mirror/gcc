/* { dg-do compile } */

int svuint16_t; /* { dg-message "note: previous declaration 'int svuint16_t'" } */

#pragma GCC aarch64 "arm_sve.h" /* { dg-error {'typedef [^'\n]* svuint16_t' redeclared as different kind of entity} } */
