/* { dg-do compile { target { ! riscv_v } } } */
/* { dg-options "-mbig-endian" } */

_Atomic enum { E0 } e;
void foo() { e++; }
