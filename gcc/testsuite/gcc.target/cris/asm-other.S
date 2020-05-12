/* { dg-do assemble } */
/* The base test-case is sort-of-disabled or rather made
   always-pass, but remains included by other tests. */

/* Make sure we can *assemble* for another variant.  */
        .text
#ifdef OTHER_ISA
0:
	move.d [$r2=$r0+42],$r1
	bwf 0b
	nop
#endif
