/* { dg-do compile } */
/* { dg-options "-march=rv64i_zhinxmin -mabi=lp64 -O3 -mcmodel=medany" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* Make sure zfhmin behaves the same way as zfh. */
/*
** foo: { target { no-opts "-flto" } }
**   lla\ta[0-9]+,\.LC0
**   lhu\ta[0-9]+,0\(a[0-9]+\)
**   ...
*/
_Float16 foo() { return 0.8974; }
