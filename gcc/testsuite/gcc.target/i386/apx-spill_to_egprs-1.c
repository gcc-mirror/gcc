/* { dg-do compile  { target { ! ia32 } } } */
/* { dg-options "-O2 -march=skylake-avx512 -mapxf -DDTYPE32" } */

#include "spill_to_mask-1.c"

/* { dg-final { scan-assembler "movl\[ \t]+\[^\\n\\r\]*, %r16d" } } */
/* { dg-final { scan-assembler "movl\[ \t]+\[^\\n\\r\]*, %r17d" } } */
/* { dg-final { scan-assembler "movl\[ \t]+\[^\\n\\r\]*, %r18d" } } */
/* { dg-final { scan-assembler "movq\[ \t]+\[^\\n\\r\]*, %r19" } } */
/* { dg-final { scan-assembler "movl\[ \t]+\[^\\n\\r\]*, %r20d" } } */
/* { dg-final { scan-assembler "movl\[ \t]+\[^\\n\\r\]*, %r21d" } } */
/* { dg-final { scan-assembler "movl\[ \t]+\[^\\n\\r\]*, %r22d" } } */
/* { dg-final { scan-assembler "movl\[ \t]+\[^\\n\\r\]*, %r23d" } } */
/* { dg-final { scan-assembler "movl\[ \t]+\[^\\n\\r\]*, %r24d" } } */
/* { dg-final { scan-assembler "addl\[ \t]+\[^\\n\\r\]*, %r25d" } } */
/* { dg-final { scan-assembler "movl\[ \t]+\[^\\n\\r\]*, %r26d" } } */
/* { dg-final { scan-assembler "movl\[ \t]+\[^\\n\\r\]*, %r27d" } } */
/* { dg-final { scan-assembler "movbel\[ \t]+\[^\\n\\r\]*, %r28d" } } */
/* { dg-final { scan-assembler "movbel\[ \t]+\[^\\n\\r\]*, %r29d" } } */
/* { dg-final { scan-assembler "movbel\[ \t]+\[^\\n\\r\]*, %r30d" } } */
/* { dg-final { scan-assembler "movbel\[ \t]+\[^\\n\\r\]*, %r31d" } } */
/* { dg-final { scan-assembler-not "knot" } } */
/* { dg-final { scan-assembler-not "kxor" } } */
/* { dg-final { scan-assembler-not "kor" } } */
/* { dg-final { scan-assembler-not "kandn" } } */
