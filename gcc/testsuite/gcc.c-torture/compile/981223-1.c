/* The problem on IA-64 is that the assembler emits

   Warning: Additional NOP may be necessary to workaround Itanium
   processor A/B step errata

   This can be fixed by adding "-mb-step" to the command line, which
   does in fact add the extra nop. */
/* { dg-options "-w -mb-step" { target ia64-*-* } } */

__complex__ float
func (__complex__ float x)
{
    if (__real__ x == 0.0)
	return 1.0;
    else
	return 0.0;
}
