/* Arrays of unknown size with element type a VLA type should not be
   initialized (C99 isn't clear about whether such arrays are VLAs,
   but this is the only reasonable interpretation).  Bug 16409, first
   testcase.  */
/* { dg-do compile } */
/* { dg-options "" } */

const int i = 1;
void foo() { char a[][i] = {""}; } /* { dg-error "variable-sized object may not be initialized" } */
