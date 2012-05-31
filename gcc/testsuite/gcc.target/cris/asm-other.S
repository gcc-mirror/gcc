/* { dg-do assemble } */
/* { dg-options "-DOTHER_ISA=0 -march=v0" { target crisv32-*-* } } */
/* { dg-options "-DOTHER_ISA=32 -march=v32" { target cris-*-* } } */

/* Make sure we can assemble for the "other" variant, with the twist
   that the gcc option -march=v0 isn't valid for the assembler.  */
        .text
#if OTHER_ISA == 32
	addoq 42,$r1,$acr
#else
0:
	move.d [$r2=$r0+42],$r1
	bwf 0b
	nop
#endif
