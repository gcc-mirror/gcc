/* { dg-do compile } */

void calculate(void) __attribute__ ((__target__ ("sve"))); /* { dg-error "arch extension 'sve' should be prefixed by '\\+'" } */
