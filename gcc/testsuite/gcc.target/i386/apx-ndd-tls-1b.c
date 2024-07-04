/* PR target/113733 */
/* { dg-do assemble { target { apxf && { ! ia32 } } } } */
/* { dg-require-effective-target tls } */
/* { dg-require-effective-target code_6_gottpoff_reloc } */
/* { dg-options "-save-temps -mapxf -O3 -w" } */

#include "apx-ndd-tls-1a.c"

/* { dg-final { scan-assembler-times "addq\[ \t]+%r\[a-z0-9\]+, a@gottpoff\\(%rip\\), %r\[a-z0-9\]+" 1 { target lp64 } } } */
