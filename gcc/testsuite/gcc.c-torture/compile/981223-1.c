/* The problem on IA-64 is that the assembler emits

   Warning: Additional NOP may be necessary to workaround Itanium
   processor A/B step errata  */

/* { dg-prune-output "Assembler messages" } */
/* { dg-prune-output "Additional NOP may be necessary" } */


__complex__ float
func (__complex__ float x)
{
    if (__real__ x == 0.0)
	return 1.0;
    else
	return 0.0;
}
