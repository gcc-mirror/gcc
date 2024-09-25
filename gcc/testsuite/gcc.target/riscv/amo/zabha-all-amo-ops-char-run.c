/* Test __atomic routines for existence on 1 byte values with each valid memory model.  */
/* { dg-do run { target { riscv_zabha } } } */
/* { dg-options "-Wno-address-of-packed-member" } */

#include "inline-atomics-3.c"
