/* { dg-do compile } */
/* { dg-additional-options "-Wno-pedantic -std=gnu90 -mfpu=64" } */

register double fs0 asm("fs0"); /* { dg-note "conflicts with 'fs0'" } */
register double f24 asm("$f24"); /* { dg-warning "register of 'f24' used for multiple global register variables" } */

void
test (void)
{
  asm("" ::: "fa0", "fa1", "fa2", "fa3", "fa4", "fa5", "fa6", "fa7",
	     "ft0", "ft1", "ft2", "ft3", "ft4", "ft5", "ft6", "ft7",
	     "ft8", "ft9", "ft10", "ft11", "ft12", "ft13", "ft14", "ft15",
	     "fs0", "fs1", "fs2", "fs3", "fs4", "fs5", "fs6", "fs7");
}
