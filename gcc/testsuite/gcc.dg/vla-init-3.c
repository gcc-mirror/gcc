/* Arrays of unknown size with element type a VLA type should not be
   initialized (C99 isn't clear about whether such arrays are VLAs,
   but this is the only reasonable interpretation).  Bug 16409, second
   testcase.  */
/* { dg-do compile } */
/* { dg-options "" } */

void foo(int i) { char a[][i] = {""}; } /* { dg-error "variable-sized object may not be initialized" } */
/* { dg-error "array size missing in 'a'" "extra error" { target *-*-* } 8 } */
