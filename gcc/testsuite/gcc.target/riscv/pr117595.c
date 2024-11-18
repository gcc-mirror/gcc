/* { dg-do compile } */
/* { dg-options "-mbig-endian" } */

_Atomic enum { E0 } e;
void foo() { e++; }
