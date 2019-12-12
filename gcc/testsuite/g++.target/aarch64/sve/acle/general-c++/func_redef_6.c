/* { dg-do compile } */

typedef int svadd_u8_x; /* { dg-message "note: previous declaration 'typedef int svadd_u8_x'" } */

#pragma GCC aarch64 "arm_sve.h" /* { dg-error {'svuint8_t svadd_u8_x\(svbool_t, svuint8_t, svuint8_t\)' redeclared as different kind of entity} } */
