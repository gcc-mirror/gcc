/* { dg-options "-O2 -fstack-protector-all -fstack-clash-protection -fomit-frame-pointer --param stack-clash-protection-guard-size=12 -fsanitize=shadow-call-stack -ffixed-x18" } */
/* { dg-skip-if "conflicts with x18" { aarch64-*-vxworks* } } */

#include "stack-check-prologue-19.c"
