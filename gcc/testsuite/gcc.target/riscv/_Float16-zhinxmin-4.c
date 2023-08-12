/* { dg-do compile } */
/* { dg-options "-march=rv64if_zfhmin -mabi=lp64 -O3 -mcmodel=medany" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* Make sure zfhmin behaves the same way as zfh. */
/*
** foo: { target { no-opts "-flto" } }
**   lh\ta0,\.LC0
**   ...
*/
_Float16 foo() { return 0.8974; }
