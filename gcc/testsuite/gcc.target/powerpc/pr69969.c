/* PR target/69969 */
/* { dg-do compile } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8" } */

int bar (int x) { return x; }
__attribute__((__target__("no-vsx"))) int foo (int x) { return x; } /* { dg-bogus "-mallow-movmisalign requires -mvsx" } */
