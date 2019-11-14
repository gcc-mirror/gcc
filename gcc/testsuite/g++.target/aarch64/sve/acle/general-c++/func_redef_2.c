/* { dg-do compile } */

int svadd_n_u8_x = 1; /* { dg-message "note: previous declaration 'int svadd_n_u8_x'" } */

#pragma GCC aarch64 "arm_sve.h" /* { dg-error {'svuint8_t svadd_n_u8_x\(svbool_t, svuint8_t, [^)\n]*\)' redeclared as different kind of entity} } */
