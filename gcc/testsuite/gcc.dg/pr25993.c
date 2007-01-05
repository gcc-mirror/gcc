/* { dg-do assemble { target i?86-*-* x86_64-*-* } } */
/* { dg-skip-if "" { "*-*-darwin*" } { "*" } { "" } } */
/* { dg-options "-std=c99 -x assembler-with-cpp" } */

#ifndef __ASSEMBLER__
extern int func(void);
#else
.global func
.type func,%function
.align 4
func:
        ret
.size func,.-func
#endif
