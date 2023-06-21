/* { dg-do compile } */
/* { dg-require-effective-target arm_hard_ok } */
/* { dg-options "-O2 -march=armv7-m -mfloat-abi=hard -mfpu=fpv4-sp-d16 -mbig-endian -mpure-code" } */
double f() { return 5.0; }
