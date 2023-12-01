/* This caused an ICE on s390x due to a bug in s390_md_asm_adjust when no
   vector extension is available.  */

/* { dg-do compile } */
/* { dg-options "-O2 -march=zEC12" } */

long double ____strtold_l_internal___x;
void ____strtold_l_internal() { __asm__("" : : "fm"(____strtold_l_internal___x)); }
