/* { dg-options "-O2" } */
void f(char *p) { asm("prfm pldl1keep, %a0\n" :: "p" (p + 6)); }
