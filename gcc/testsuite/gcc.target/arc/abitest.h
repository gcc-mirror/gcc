#ifndef _DG_ABITEST_H
#define _DG_ABITEST_H 1

#ifdef  __ASSEMBLER__

#define ENTRY(nm)               \
        .text `                 \
        .align 4 `              \
        .globl nm `             \
        .type nm,@function `    \
nm:

#define END(name)       .size name,.-name

#endif /* __ASSEMBLER __*/

#endif /*_DG_ABITEST_H*/
