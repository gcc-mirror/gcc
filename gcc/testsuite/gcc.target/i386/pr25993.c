/* { dg-do assemble } */
/* { dg-skip-if "" { "*-*-darwin*" "*-*-mingw*" } } */
/* { dg-options "-std=c99 -x assembler-with-cpp" } */

#ifndef __ASSEMBLER__
extern int func(void);
#else
#ifdef __sun__
.globl func
#else
.global func
#endif
.type func,@function
.align 4
func:
        ret
.size func,.-func
#endif
