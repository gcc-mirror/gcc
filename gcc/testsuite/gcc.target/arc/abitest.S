/* { dg-do assemble } */
#ifndef ENTRY
#define ENTRY(nm)               \
        .text `                 \
        .align 4 `              \
        .globl nm `             \
        .type nm,@function `    \
nm:
#endif

#ifndef END
#define END(name)       .size name,.-name
#endif

ENTRY(tsyscall)
ENTRY(clone)
	add	r0,r0,r1
	add	r0,r0,r2
	add	r0,r0,r3
	add	r0,r0,r4
	add	r0,r0,r5
	j_s.d	[blink]
	add	r0,r0,r6
END(tsyscall)
END(clone)

ENTRY(abidi)
	add.f r0,r1,1
        j_s.d [blink]
        adc r1,r2,0
END(abidi)
