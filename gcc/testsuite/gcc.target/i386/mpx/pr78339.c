/* { dg-do compile } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -Wsuggest-attribute=noreturn" } */

extern _Noreturn void exit (int);
int main (void) { exit (1); }
