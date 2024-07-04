/* { dg-do compile } */
/* { dg-options "-Os -fharden-control-flow-redundancy -fnon-call-exceptions" } */
_Complex long *c;
void init() { *c = 1.0; }
