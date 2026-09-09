/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -macev1" } */

void ace (char c)
{
  __builtin_ia32_tilezero (c); /* { dg-error "the tmm register number argument must be between 0 to 7" "" { target { ! ia32 } } 0 } */
}
