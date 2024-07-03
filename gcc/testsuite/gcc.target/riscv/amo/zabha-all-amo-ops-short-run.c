/* Test __atomic routines for execution on 2 byte values with each valid memory model.  */
/* { dg-do run { target { riscv_zabha } } } */
/* { dg-options "-Wno-address-of-packed-member" } */

#include "inline-atomics-4.c"
