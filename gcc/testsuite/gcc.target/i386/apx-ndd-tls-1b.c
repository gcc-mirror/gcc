/* PR target/113733 */
/* { dg-do assemble { target { apxf && { ! ia32 } } } } */
/* { dg-require-effective-target tls } */
/* { dg-require-effective-target code_6_gottpoff_reloc } */
/* { dg-options "-save-temps -std=gnu17 -mapxf -O3 -w" } */
/* The testcase is fragile, it's supposed to check the compiler
   ability of generating code_6_gottpoff_reloc instruction, but
   failed since there's a seg_prefixed memory
   usage(r14-6242-gd564198f960a2f).  */
#include "apx-ndd-tls-1a.c"

/* { dg-final { scan-assembler-times "addq\[ \t]+%r\[a-z0-9\]+, a@gottpoff\\(%rip\\), %r\[a-z0-9\]+" 1 { xfail lp64 } } } */
