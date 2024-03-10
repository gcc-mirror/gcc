/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zfhmin -mabi=lp64d -O3 -mcmodel=medany" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* Make sure zfhmin behaves the same way as zfh. */
/*
** foo: { target { no-opts "-flto" } }
**   flh\tfa0,\.LC0,[a-z0-9]+
**   ...
*/
_Float16 foo() { return 0.8974; }
